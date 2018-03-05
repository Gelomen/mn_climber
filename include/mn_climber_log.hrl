-ifndef(MN_CLIMBER_LOG_H).
-define(MN_CLIMBER_LOG_H, true).

-define(DEBUG(Msg),                     lager:debug(Msg)).
-define(DEBUG(Format, Args),            lager:debug(Format, Args)).
-define(DEBUG(Format, Args, Opts),      lager:debug(Format, Args, Opts)).

-define(INFO(Msg),                      lager:info(Msg)).
-define(INFO(Format, Args),             lager:info(Format, Args)).
-define(INFO(Format, Args, Opts),       lager:info(Format, Args, Opts)).

-define(NOTICE(Msg),                    lager:notice(Msg)).
-define(NOTICE(Format, Args),           lager:notice(Format, Args)).
-define(NOTICE(Format, Args, Opts),     lager:notice(Format, Args, Opts)).

-define(WARNING(Msg),                   lager:warning(Msg)).
-define(WARNING(Format, Args),          lager:warning(Format, Args)).
-define(WARNING(Format, Args, Opts),    lager:warning(Format, Args, Opts)).

-define(ERROR(Msg),                     lager:error(Msg)).
-define(ERROR(Format, Args),            lager:error(Format, Args)).
-define(ERROR(Format, Args, Opts),      lager:error(Format, Args, Opts)).

-define(CRITICAL(Msg),                  lager:critical(Msg)).
-define(CRITICAL(Format, Args),         lager:critical(Format, Args)).
-define(CRITICAL(Format, Args, Opts),   lager:critical(Format, Args, Opts)).

-define(ALERT(Msg),                     lager:alert(Msg)).
-define(ALERT(Format, Args),            lager:alert(Format, Args)).
-define(ALERT(Format, Args, Opts),      lager:alert(Format, Args, Opts)).

-define(EMERGENCY(Msg),                 lager:emergency(Msg)).
-define(EMERGENCY(Format, Args),        lager:emergency(Format, Args)).
-define(EMERGENCY(Format, Args, Opts),  lager:emergency(Format, Args, Opts)).

-endif.