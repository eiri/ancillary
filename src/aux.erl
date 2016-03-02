-module(aux).

-compile({no_auto_import, [error/2]}).

-export([debug/1, debug/2, info/1, info/2]).
-export([warning/1, warning/2, error/1, error/2]).

-define(MGR, error_logger).

%%====================================================================
%% API functions
%%====================================================================

debug(Msg) ->
  debug(Msg, []).

debug(Fmt, Args) ->
  notify(debug_msg, Fmt, Args).

info(Msg) ->
  info(Msg, []).

info(Fmt, Args) ->
  notify(info_msg, Fmt, Args).

warning(Msg) ->
  warning(Msg, []).

warning(Fmt, Args) ->
  notify(warning_msg, Fmt, Args).

error(Msg) ->
  error(Msg, []).

error(Fmt, Args) ->
  notify(error, Fmt, Args).

%%====================================================================
%% Internal functions
%%====================================================================

notify(Tag, Fmt, Args) ->
  Msg = {Tag, group_leader(), {self(), Fmt, Args}},
  gen_event:notify(?MGR, Msg).