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

-module(mn_climber_mnesia).

-export([
    init/0,
    reset/0,
    force_load_next_boot/0,

    node_type/0,
    dir/0,

    copy_db/1,
    ensure_mnesia_dir/0
]).

-include("mn_climber_log.hrl").

-export_type([node_type/0]).
-type node_type() :: disc | ram.

%%----------------------------------------------------------------------------
%% Main interface
%%----------------------------------------------------------------------------

-spec init() -> 'ok'.
init() ->
    ensure_mnesia_running(),
    ensure_mnesia_dir(),
    case is_virgin_node() of
        true ->
            ?INFO("Database directory at ~s is empty. Initialising from scratch...~n",
                [dir()]),
            init_from_config();
        false ->
            ensure_schema_integrity(),
            %% ...and need to wait for tables
            mn_climber_table:wait_for_replicated(true)
    end,
    ok.

init_from_config() ->
    ok = create_schema().

%% return node to its virgin state, where it is not member of any
%% cluster, has no cluster configuration, no local database, and no
%% persisted messages
-spec reset() -> 'ok'.
reset() ->
    ensure_mnesia_not_running(),
    ?INFO("Resetting Rabbit~n", []),
    mn_climber_misc:ensure_ok(mnesia:delete_schema([node()]), cannot_delete_schema),
    wipe().

wipe() ->
    %% remove persisted messages and any other garbage we find
    ok = mn_climber_file:recursive_delete(filelib:wildcard(dir() ++ "/*")),
    ok.

%%----------------------------------------------------------------------------
%% Queries
%%----------------------------------------------------------------------------

-spec node_type() -> node_type().
node_type() ->
    case mnesia:system_info(use_dir) of
        true -> disc;
        false -> ram
    end.

-spec dir() -> file:filename().
dir() -> mnesia:system_info(directory).

%%----------------------------------------------------------------------------
%% Operations on the db
%%----------------------------------------------------------------------------

-spec ensure_mnesia_dir() -> 'ok'.
ensure_mnesia_dir() ->
    MnesiaDir = dir() ++ "/",
    case filelib:ensure_dir(MnesiaDir) of
        {error, Reason} ->
            throw({error, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
        ok ->
            ok
    end.

ensure_mnesia_running() ->
    case mnesia:system_info(is_running) of
        yes ->
            ok;
        starting ->
            wait_for(mnesia_running),
            ensure_mnesia_running();
        Reason when Reason =:= no; Reason =:= stopping ->
            throw({error, mnesia_not_running})
    end.

ensure_mnesia_not_running() ->
    case mnesia:system_info(is_running) of
        no ->
            ok;
        stopping ->
            wait_for(mnesia_not_running),
            ensure_mnesia_not_running();
        Reason when Reason =:= yes; Reason =:= starting ->
            throw({error, mnesia_unexpectedly_running})
    end.

ensure_schema_integrity() ->
    case mn_climber_table:check_schema_integrity(_Retry = true) of
        ok ->
            ok;
        {error, Reason} ->
            throw({error, {schema_integrity_check_failed, Reason}})
    end.

-spec copy_db(file:filename()) -> rabbit_types:ok_or_error(any()).
copy_db(Destination) ->
    ok = ensure_mnesia_not_running(),
    mn_climber_file:recursive_copy(dir(), Destination).

force_load_filename() ->
    filename:join(dir(), "force_load").

-spec force_load_next_boot() -> 'ok'.
force_load_next_boot() ->
    file:write_file(force_load_filename(), <<"">>).

%%maybe_force_load() ->
%%    case mn_climber_file:is_file(force_load_filename()) of
%%        true ->
%%            mn_climber_table:force_load(),
%%            file:delete(force_load_filename());
%%        false -> ok
%%    end.

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

%%schema_ok_or_move() ->
%%    case mn_climber_table:check_schema_integrity(_Retry = false) of
%%        ok ->
%%            ok;
%%        {error, Reason} ->
%%            %% NB: we cannot use rabbit_log here since it may not have been
%%            %% started yet
%%            ?WARNING("schema integrity check failed: ~p~n"
%%            "moving database to backup location "
%%            "and recreating schema from scratch~n",
%%                [Reason]),
%%            ok = move_db(),
%%            ok = create_schema()
%%    end.

%% We only care about disc nodes since ram nodes are supposed to catch
%% up only
create_schema() ->
    stop_mnesia(),
    mn_climber_misc:ensure_ok(mnesia:create_schema([node()]), cannot_create_schema),
    start_mnesia(),
    ok = mn_climber_table:create(),
    ensure_schema_integrity(),
    ok = mn_climber_version:record_desired().

%%move_db() ->
%%    stop_mnesia(),
%%    MnesiaDir = filename:dirname(dir() ++ "/"),
%%    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
%%    BackupDir = mn_climber_misc:format(
%%        "~s_~w~2..0w~2..0w~2..0w~2..0w~2..0w",
%%        [MnesiaDir, Year, Month, Day, Hour, Minute, Second]),
%%    case file:rename(MnesiaDir, BackupDir) of
%%        ok ->
%%            %% NB: we cannot use rabbit_log here since it may not have
%%            %% been started yet
%%            ?WARNING("moved database from ~s to ~s~n",
%%                [MnesiaDir, BackupDir]),
%%            ok;
%%        {error, Reason} -> throw({error, {cannot_backup_mnesia,
%%            MnesiaDir, BackupDir, Reason}})
%%    end,
%%    ensure_mnesia_dir(),
%%    start_mnesia(),
%%    ok.

wait_for(Condition) ->
    ?INFO("Waiting for ~p...~n", [Condition]),
    timer:sleep(1000).

start_mnesia() ->
    mn_climber_misc:ensure_ok(mnesia:start(), cannot_start_mnesia),
    ensure_mnesia_running().

stop_mnesia() ->
    stopped = mnesia:stop(),
    ensure_mnesia_not_running().

is_virgin_node() ->
    case prim_file:list_dir(dir()) of
        {error, enoent} -> true;
        {ok, []} -> true;
        {ok, _} -> false
    end.