-module(aux_logger).

-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).

-record(ctx, {writer, formatter}).

%%====================================================================
%% Callback functions
%%====================================================================

init(_Args) ->
  Writer = make_writer(),
  Formatter = make_formatter(),
  {ok, #ctx{writer=Writer, formatter=Formatter}}.

handle_event({Type, Gleader, {Pid, Fmt, Args}}, State) ->
  case type(Type, Fmt) of
    msg ->
      Opts = [{gleader, Gleader}, {pid, Pid}, {type, Type}],
      display(Fmt, Args, Opts, State);
    _ ->
      ok
  end,
  {ok, State}.

terminate(_Args, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

make_writer() ->
  fun(Msg) ->
    io:format(standard_error, "~s~n", [Msg])
  end.

make_formatter() ->
  fun(Opts) ->
    Args = [get_value(K, Opts) || K <- [time, pid, type, message]],
    io_lib:format("~s ~p [~s] ~s", Args)
  end.

format_time({_, _, MSec} = Now) ->
  {{Y, M, D}, {Hr, Min, Sec}} = calendar:now_to_local_time(Now),
  io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B"
    " ~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
    [Y, M, D, Hr, Min, Sec, MSec div 1000]).

get_value(Key, Opts) ->
  case lists:keyfind(Key, 1, Opts) of
    {Key, Value} -> Value;
    false -> undefined
  end.

display(Fmt, Args, Opts0, #ctx{formatter=Formatter, writer=Writer}) ->
  spawn(fun() ->
    Time = format_time(os:timestamp()),
    Opts = [{message, io_lib:format(Fmt, Args)}, {time, Time}|Opts0],
    Msg = Formatter(Opts),
    Writer(Msg)
  end).

type(_, crash_report) -> report;
type(_, supervisor_report) -> report;
type(_, supervisor) -> report;
type(_, progress_report) -> report;
type(_, progress) -> report;
type(_, std_info) -> report;
type(_, std_warning) -> report;
type(_, std_error) -> report;
type(debug_msg, _) -> msg;
type(error, _) -> msg;
type(error_report, _) -> report;
type(warning_msg, _) -> msg;
type(warning_report, _) -> msg;
type(info_msg, _) -> msg;
type(info_report, _) -> report;
type(_, _) -> unknown.
