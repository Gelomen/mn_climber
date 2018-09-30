%%%-------------------------------------------------------------------
%%% @doc
%%% application
%%% @end
%%%-------------------------------------------------------------------
-module(mn_climber_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include("mn_climber_log.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    % make sure mnesia stop
    stopped = mnesia:stop(),
    % new ets for insert table definitions
    ets:new(mn_climber_table, [named_table, public, set]),
    case mn_climber_misc:is_empty_apps() of
        true ->
            ?WARNING("!!! empty_apps !!!"),
            ?WARNING("Please add {use_mn_climber, true} to your app env");
        _ ->
            Definitions = mn_climber_table:get_definitions_from_modules(),
            ets:insert(mn_climber_table, Definitions)
    end,
    % auto_upgrade
    case application:get_env(mn_climber, auto_upgrade) of
        {ok, true} ->
            mn_climber_upgrade:maybe_upgrade_mnesia();
        _ -> none
    end,
    application:ensure_all_started(mnesia),
    % auto_init
    case application:get_env(mn_climber, auto_init) of
        {ok, true} ->
            mn_climber_mnesia:init();
        _ -> none
    end,
    % delete upgrade backup data after ensure_schema_integrity
    mn_climber_upgrade:ensure_backup_removed(),
    mn_climber_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
