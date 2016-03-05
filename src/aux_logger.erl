-module(aux_logger).

-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).

-record(ctx, {writer, formatter}).

%%====================================================================
%% Callback functions
%%====================================================================

init(Args) ->
  Writer = make_writer(),
  Format = proplists:get_value(format, Args, "{time} - {message}"),
  Formatter = make_formatter(Format),
  {ok, #ctx{writer=Writer, formatter=Formatter}}.

handle_event({Type, Gleader, {Pid, Fmt, Args}}, State) ->
  case type_severity(Type, Fmt) of
    {msg, Severity} ->
      Opts = [
        {pid, Pid},
        {gleader, Gleader},
        {type, message},
        {severity, Severity}
      ],
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

make_formatter(Format) ->
  {ok, Fmt, Keys} = make_format(Format),
  fun(Opts) ->
    Args = [get_value(K, Opts) || K <- Keys],
    io_lib:format(Fmt, Args)
  end.

make_format(Format) ->
  {ok, Tokens, _} = erl_scan:string(Format, 1, [return_white_spaces]),
  make_format(Tokens, {[], [], false}).

make_format([], {Fmt, Keys, _}) ->
  {ok, lists:concat(lists:reverse(Fmt)), lists:reverse(Keys)};
make_format([Token|Rest], {Fmt, Keys, WriteFlag}) ->
  case erl_scan:symbol(Token) of
    '{' ->
      make_format(Rest, {Fmt, Keys, true});
    '}' ->
      make_format(Rest, {Fmt, Keys, false});
    Char when WriteFlag ->
      case erl_scan:category(Token) of
        white_space ->
          make_format(Rest, {Fmt, Keys, true});
        atom ->
          make_format(Rest, {[map_key(Char)|Fmt], [Char|Keys], true})
      end;
    Char ->
      make_format(Rest, {[Char|Fmt], Keys, false})
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

type_severity(_, crash_report) -> {report, error};
type_severity(_, supervisor_report) -> {report, info};
type_severity(_, supervisor) -> {report, info};
type_severity(_, progress_report) -> {report, info};
type_severity(_, progress) -> {report, info};
type_severity(_, std_info) -> {report, info};
type_severity(_, std_warning) -> {report, warning};
type_severity(_, std_error) -> {report, error};
type_severity(debug_msg, _) -> {msg, debug};
type_severity(error, _) -> {msg, error};
type_severity(error_report, _) -> {report, error};
type_severity(warning_msg, _) -> {msg, warning};
type_severity(warning_report, _) -> {report, warning};
type_severity(info_msg, _) -> {msg, info};
type_severity(info_report, _) -> {report, info};
type_severity(_, _) -> {unknown, error}.

map_key(pid) -> "~p";
map_key(gleader) -> "~p";
map_key(type) -> "~s";
map_key(severity) -> "~s";
map_key(time) -> "~s";
map_key(message) -> "~s".

%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_format_test_() ->
  [
    {"Parse an empty string", ?_assertEqual(
      {ok, "", []},
      make_format("")
    )},
    {"Parse no vars string", ?_assertEqual(
      {ok, "pid time message", []},
      make_format("pid time message")
    )},
    {"Parse one var string", ?_assertEqual(
      {ok, "time ~p message", [pid]},
      make_format("time {pid} message")
    )},
    {"Parse multi vars string", ?_assertEqual(
      {ok, "~s (~p) - ~s", [time, pid, message]},
      make_format("{time} ({pid}) - {message}")
    )},
    {"Tolerate integers in a format string", ?_assertEqual(
      {ok, "~p - 21 - ~s", [pid, message]},
      make_format("{pid} - 21 - {message}")
    )},
    {"Preserve spaces", ?_assertEqual(
      {ok, "~s   (~p)   -   ~s", [time, pid, message]},
      make_format("{time}   ({pid})   -   {message}")
    )},
    {"Ignore spaces in vars", ?_assertEqual(
      {ok, "~s - ~s", [time, message]},
      make_format("{ time  } - {message}")
    )},
    {"Recognize all keys", ?_assertMatch(
      {ok, _, [pid, gleader, type, severity, time, message]},
      make_format("{pid}{gleader}{type}{severity}{time}{message}")
    )}
  ].

-endif.
