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

-module(mn_climber_upgrade).

-export([
    maybe_upgrade_mnesia/0,
    ensure_backup_removed/0
]).

-include("mn_climber_log.hrl").

-define(VERSION_FILENAME, "schema_version").
-define(LOCK_FILENAME, "schema_upgrade_lock").

%% -------------------------------------------------------------------

%% The upgrade logic is quite involved, due to the existence of
%% clusters.
%%
%% Firstly, we have two different types of upgrades to do: Mnesia and
%% everythinq else. Mnesia upgrades must only be done by one node in
%% the cluster (we treat a non-clustered node as a single-node
%% cluster). This is the primary upgrader. The other upgrades need to
%% be done by all nodes.
%%
%% The primary upgrader has to start first (and do its Mnesia
%% upgrades). Secondary upgraders need to reset their Mnesia database
%% and then rejoin the cluster. They can't do the Mnesia upgrades as
%% well and then merge databases since the cookie for each table will
%% end up different and the merge will fail.
%%
%% This in turn means that we need to determine whether we are the
%% primary or secondary upgrader *before* Mnesia comes up. If we
%% didn't then the secondary upgrader would try to start Mnesia, and
%% either hang waiting for a node which is not yet up, or fail since
%% its schema differs from the other nodes in the cluster.
%%
%% Also, the primary upgrader needs to start Mnesia to do its
%% upgrades, but needs to forcibly load tables rather than wait for
%% them (in case it was not the last node to shut down, in which case
%% it would wait forever).
%%
%% This in turn means that maybe_upgrade_mnesia/0 has to be patched
%% into the boot process by prelaunch before the mnesia application is
%% started. By the time Mnesia is started the upgrades have happened
%% (on the primary), or Mnesia has been reset (on the secondary) and
%% rabbit_mnesia:init_db_unchecked/2 can then make the node rejoin the cluster
%% in the normal way.
%%
%% The non-mnesia upgrades are then triggered by
%% rabbit_mnesia:init_db_unchecked/2. Of course, it's possible for a given
%% upgrade process to only require Mnesia upgrades, or only require
%% non-Mnesia upgrades. In the latter case no Mnesia resets and
%% reclusterings occur.
%%
%% The primary upgrader needs to be a disc node. Ideally we would like
%% it to be the last disc node to shut down (since otherwise there's a
%% risk of data loss). On each node we therefore record the disc nodes
%% that were still running when we shut down. A disc node that knows
%% other nodes were up when it shut down, or a ram node, will refuse
%% to be the primary upgrader, and will thus not start when upgrades
%% are needed.
%%
%% However, this is racy if several nodes are shut down at once. Since
%% rabbit records the running nodes, and shuts down before mnesia, the
%% race manifests as all disc nodes thinking they are not the primary
%% upgrader. Therefore the user can remove the record of the last disc
%% node to shut down to get things going again. This may lose any
%% mnesia changes that happened after the node chosen as the primary
%% upgrader was shut down.

%% -------------------------------------------------------------------

ensure_backup_taken() ->
    case filelib:is_file(lock_filename()) of
        false ->
            case filelib:is_dir(backup_dir()) of
                false ->
                    ok = take_backup();
                _ -> ok
            end;
        true ->
            error("Found lock file at ~s.
            Either previous upgrade is in progress or has failed.
            Database backup path: ~s", [lock_filename(), backup_dir()]),
            throw({error, previous_upgrade_failed})
    end.

take_backup() ->
    BackupDir = backup_dir(),
    case mn_climber_mnesia:copy_db(BackupDir) of
        ok ->
            ?INFO("upgrades: Mnesia dir backed up to ~p~n", [BackupDir]);
        {error, E} ->
            throw({could_not_back_up_mnesia_dir, E, BackupDir})
    end.

ensure_backup_removed() ->
    case filelib:is_dir(backup_dir()) of
        true ->
            ok = remove_backup();
        _ -> ok
    end.

remove_backup() ->
    ok = mn_climber_file:recursive_delete([backup_dir()]),
    ?INFO("upgrades: Mnesia backup removed~n", []).

-spec maybe_upgrade_mnesia() -> 'ok'.
maybe_upgrade_mnesia() ->
    ok = mn_climber_mnesia_rename:maybe_finish(),
    case mn_climber_version:upgrades_required() of
        {error, starting_from_scratch} ->
            ok;
        {error, _} = Err ->
            throw(Err);
        {ok, []} ->
            ok;
        {ok, Upgrades} ->
            ensure_backup_taken(),
            ok = apply_upgrades(Upgrades, fun mn_climber_table:force_load/0)
    end.

%% -------------------------------------------------------------------

apply_upgrades(Upgrades, Fun) ->
    ok = mn_climber_file:lock_file(lock_filename()),
    ?INFO("mnesia upgrades: ~w to apply~n", [length(Upgrades)]),
    mn_climber_misc:ensure_ok(mnesia:start(), cannot_start_mnesia),
    Fun(),
    [apply_upgrade(Upgrade) || Upgrade <- Upgrades],
    ?INFO("mnesia upgrades: All upgrades applied successfully~n"),
    ok = mn_climber_version:record_desired(),
    ok = file:delete(lock_filename()).

apply_upgrade({M, F}) ->
    ?INFO("mnesia upgrades: Applying ~w:~w~n", [M, F]),
    ok = apply(M, F, []).

%% -------------------------------------------------------------------

dir() -> mn_climber_mnesia:dir().

lock_filename() -> lock_filename(dir()).
lock_filename(Dir) -> filename:join(Dir, ?LOCK_FILENAME).
backup_dir() -> dir() ++ "-upgrade-backup".
