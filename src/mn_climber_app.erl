%%%-------------------------------------------------------------------
%%% @doc
%%%
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
    case application:get_env(mn_climber, auto_upgrade) of
        {ok, true} ->
            mn_climber_upgrade:maybe_upgrade_mnesia();
        _ -> none
    end,
    application:ensure_all_started(mnesia),
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
