-module(aux).

-compile({no_auto_import, [error/2]}).

-export([add_handler/0, add_handler/1, swap_handler/0, swap_handler/1]).
-export([debug/1, debug/2, info/1, info/2]).
-export([warning/1, warning/2, error/1, error/2]).

-define(MGR, error_logger).

%%====================================================================
%% API functions
%%====================================================================

add_handler() ->
  add_handler([]).

add_handler(Opts) ->
  gen_event:add_handler(?MGR, aux_logger, Opts).

swap_handler() ->
  swap_handler([]).

swap_handler(Opts) ->
  lists:foreach(fun(Handler) ->
    gen_event:delete_handler(?MGR, Handler, [])
  end, gen_event:which_handlers(?MGR)),
  add_handler(Opts).

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
  case catch io_lib_format:fwrite(Fmt, Args) of
    {'EXIT', _} -> erlang:error(badarg, [Fmt, Args]);
    _ ->
      Msg = {Tag, group_leader(), {self(), Fmt, Args}},
      gen_event:notify(?MGR, Msg)
  end.
