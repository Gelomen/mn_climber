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

-module(mn_climber_table).

-export([
    create/0, wait_for_replicated/1, wait/1,
    force_load/0, is_present/0, is_empty/0,
    check_schema_integrity/1, clear_ram_only_tables/0, retry_timeout/0,
    wait_for_replicated/0, definitions/0
]).

-include("mn_climber_log.hrl").

%%----------------------------------------------------------------------------
-type retry() :: boolean().

-callback table_definitions() -> list().

%%----------------------------------------------------------------------------
%% Main interface
%%----------------------------------------------------------------------------

-spec create() -> 'ok'.
create() ->
    lists:foreach(
        fun({Tab, TabDef}) ->
            create_table(Tab, TabDef)
        end, definitions()),
    ok.

-spec create_table(atom(), list()) -> ok.
create_table(Tab, TabDef) ->
    TabDef1 = proplists:delete(match, TabDef),
    case mnesia:create_table(Tab, TabDef1) of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            throw({error, {table_creation_failed, Tab, TabDef1, Reason}})
    end.

%% This arity only exists for backwards compatibility with certain
%% plugins. See https://github.com/rabbitmq/rabbitmq-clusterer/issues/19.
-spec wait_for_replicated() -> 'ok'.
wait_for_replicated() ->
    wait_for_replicated(false).

-spec wait_for_replicated(retry()) -> 'ok'.
wait_for_replicated(Retry) ->
    wait([Tab || {Tab, _TabDef} <- definitions()], Retry).

-spec wait([atom()]) -> 'ok'.
wait(TableNames) ->
    wait(TableNames, _Retry = false).

wait(TableNames, Retry) ->
    {Timeout, Retries} = retry_timeout(Retry),
    wait(TableNames, Timeout, Retries).

wait(TableNames, Timeout, Retries) ->
    %% We might be in ctl here for offline ops, in which case we can't
    %% get_env() for the rabbit app.
    ?INFO("Waiting for Mnesia tables for ~p ms, ~p retries left~n",
        [Timeout, Retries - 1]),
    Result =
        case mnesia:wait_for_tables(TableNames, Timeout) of
            ok ->
                ok;
            {timeout, BadTabs} ->
                {error, {timeout_waiting_for_tables, BadTabs}};
            {error, Reason} ->
                {error, {failed_waiting_for_tables, Reason}}
        end,
    case {Retries, Result} of
        {_, ok} ->
            ok;
        {1, {error, _} = Error} ->
            throw(Error);
        {_, {error, Error}} ->
            ?WARNING("Error while waiting for Mnesia tables: ~p~n", [Error]),
            wait(TableNames, Timeout, Retries - 1);
        _ ->
            wait(TableNames, Timeout, Retries - 1)
    end.

retry_timeout(_Retry = false) ->
    {retry_timeout(), 1};
retry_timeout(_Retry = true) ->
    Retries =
        case application:get_env(mn_climber, mnesia_table_loading_retry_limit) of
            {ok, T} -> T;
            undefined -> 10
        end,
    {retry_timeout(), Retries}.

-spec retry_timeout() -> {non_neg_integer() | infinity, non_neg_integer()}.
retry_timeout() ->
    case application:get_env(mn_climber, mnesia_table_loading_retry_timeout) of
        {ok, T} -> T;
        undefined -> 30000
    end.

-spec force_load() -> 'ok'.
force_load() -> [mnesia:force_load_table(T) || T <- names()], ok.

-spec is_present() -> boolean().
is_present() -> names() -- mnesia:system_info(tables) =:= [].

-spec is_empty() -> boolean().
is_empty() -> is_empty(names()).

is_empty(Names) ->
    lists:all(fun(Tab) -> mnesia:dirty_first(Tab) == '$end_of_table' end, Names).

-spec check_schema_integrity(retry()) -> rabbit_types:ok_or_error(any()).
check_schema_integrity(Retry) ->
    Tables = mnesia:system_info(tables),
    CheckTabDefResult = check(
        fun(Tab, TabDef) ->
            case lists:member(Tab, Tables) of
                false ->
                    create_table(Tab, TabDef);
                true ->
                    check_attributes(Tab, TabDef)
            end
        end),
    case CheckTabDefResult of
        ok ->
            wait(names(), Retry),
            check(fun check_content/2);
        Other -> Other
    end.

-spec clear_ram_only_tables() -> 'ok'.
clear_ram_only_tables() ->
    Node = node(),
    lists:foreach(
        fun(TabName) ->
            case lists:member(Node, mnesia:table_info(TabName, ram_copies)) of
                true -> {atomic, ok} = mnesia:clear_table(TabName);
                false -> ok
            end
        end, names()),
    ok.

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

check_attributes(Tab, TabDef) ->
    {_, ExpAttrs} = proplists:lookup(attributes, TabDef),
    case mnesia:table_info(Tab, attributes) of
        ExpAttrs -> ok;
        Attrs -> {error, {table_attributes_mismatch, Tab, ExpAttrs, Attrs}}
    end.

check_content(Tab, TabDef) ->
    {_, Match} = proplists:lookup(match, TabDef),
    case mnesia:dirty_first(Tab) of
        '$end_of_table' ->
            ok;
        Key ->
            ObjList = mnesia:dirty_read(Tab, Key),
            MatchComp = ets:match_spec_compile([{Match, [], ['$_']}]),
            case ets:match_spec_run(ObjList, MatchComp) of
                ObjList -> ok;
                _ -> {error, {table_content_invalid, Tab, Match, ObjList}}
            end
    end.

check(Fun) ->
    case [Error || {Tab, TabDef} <- definitions(),
        begin
            {Ret, Error} =
                case Fun(Tab, TabDef) of
                    ok -> {false, none};
                    {error, E} -> {true, E}
                end,
            Ret
        end] of
        [] -> ok;
        Errors -> {error, Errors}
    end.

%%--------------------------------------------------------------------
%% Table definitions
%%--------------------------------------------------------------------

names() -> [Tab || {Tab, _} <- definitions()].

definitions() ->
    lists:append([Module:table_definitions() || Module <-
        mn_climber_misc:all_attr_modules(behavior, [?MODULE]) ++
        mn_climber_misc:all_attr_modules(behaviour, [?MODULE])]).