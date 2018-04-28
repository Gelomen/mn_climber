-module(mn_climber).

%% API exports
-export([
    definitions/0
]).

%%====================================================================
%% API functions
%%====================================================================
-spec definitions() -> mn_climber_table:table_definitions().
definitions() ->
    ets:tab2list(mn_climber_table).

%%====================================================================
%% Internal functions
%%====================================================================
