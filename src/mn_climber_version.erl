%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2017 Pivotal Software, Inc.  All rights reserved.
%%

-module(mn_climber_version).

-export([
    recorded/0, matches/2, desired/0,
    record_desired/0, upgrades_required/0
]).

%% -------------------------------------------------------------------

-export_type([step/0]).

-type step() :: {atom(), atom()}.
-type version() :: [atom()].

-define(VERSION_FILENAME, "schema_version").

%% -------------------------------------------------------------------

-spec recorded() -> rabbit_types:ok_or_error2(version(), any()).
recorded() ->
    case mn_climber_file:read_term_file(schema_filename()) of
        {ok, [V]} -> {ok, V};
        {error, _} = Err -> Err
    end.

record(V) ->
    ok = mn_climber_file:write_term_file(schema_filename(), [V]).

recorded_for_upgrades() ->
    case recorded() of
        {error, _} = Err ->
            Err;
        {ok, Version} ->
            {ok, get_upgrades(Version)}
    end.

%% -------------------------------------------------------------------

-spec matches([A], [A]) -> boolean().
matches(VerA, VerB) ->
    lists:usort(VerA) =:= lists:usort(VerB).

%% -------------------------------------------------------------------

-spec desired() -> version().
desired() -> with_upgrade_graph(fun heads/1).

-spec record_desired() -> 'ok'.
record_desired() -> record(desired()).

-spec upgrades_required() -> rabbit_types:ok_or_error2([step()], any()).
upgrades_required() ->
    case recorded_for_upgrades() of
        {error, enoent} ->
            {error, starting_from_scratch};
        {ok, CurrentHeads} ->
            with_upgrade_graph(
                fun(G) ->
                    case unknown_heads(CurrentHeads, G) of
                        [] -> {ok, upgrades_to_apply(CurrentHeads, G)};
                        Unknown -> {error, {future_upgrades_found, Unknown}}
                    end
                end)
    end.

%% -------------------------------------------------------------------

with_upgrade_graph(Fun) ->
    case mn_climber_misc:build_acyclic_graph(
        fun({_App, Module, Steps}) -> vertices(Module, Steps) end,
        fun({_App, Module, Steps}) -> edges(Module, Steps) end,
        mn_climber_misc:all_module_attributes(mn_climber_upgrade)) of
        {ok, G} ->
            try
                Fun(G)
            after
                true = digraph:delete(G)
            end;
        {error, {vertex, duplicate, StepName}} ->
            throw({error, {duplicate_upgrade_step, StepName}});
        {error, {edge, {bad_vertex, StepName}, _From, _To}} ->
            throw({error, {dependency_on_unknown_upgrade_step, StepName}});
        {error, {edge, {bad_edge, StepNames}, _From, _To}} ->
            throw({error, {cycle_in_upgrade_steps, StepNames}})
    end.

vertices(Module, Steps) ->
    [{StepName, {Module, StepName}} || {StepName, _Reqs} <- Steps].

edges(_Module, Steps) ->
    [{Require, StepName} || {StepName, Requires} <- Steps,
        Require <- Requires].
unknown_heads(Heads, G) ->
    [H || H <- Heads, digraph:vertex(G, H) =:= false].

upgrades_to_apply(Heads, G) ->
    %% Take all the vertices which can reach the known heads. That's
    %% everything we've already applied. Subtract that from all
    %% vertices: that's what we have to apply.
    Unsorted = sets:to_list(
        sets:subtract(
            sets:from_list(digraph:vertices(G)),
            sets:from_list(digraph_utils:reaching(Heads, G)))),
    %% Form a subgraph from that list and find a topological ordering
    %% so we can invoke them in order.
    [element(2, digraph:vertex(G, StepName)) ||
        StepName <- digraph_utils:topsort(digraph_utils:subgraph(G, Unsorted))].

heads(G) ->
    lists:sort([V || V <- digraph:vertices(G), digraph:out_degree(G, V) =:= 0]).

%% -------------------------------------------------------------------

get_upgrades(Version) when is_list(Version) ->
    [Name || {_App, _Module, Attributes} <-
        mn_climber_misc:all_module_attributes(mn_climber_upgrade),
        {Name, _Requires} <- Attributes,
        lists:member(Name, Version)].

dir() -> mn_climber_mnesia:dir().

schema_filename() -> filename:join(dir(), ?VERSION_FILENAME).