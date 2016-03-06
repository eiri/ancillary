-module(aux_logger).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_info/2, terminate/2]).

-record(ctx, {writer, formatter, queue=dict:new()}).

%%====================================================================
%% Callback functions
%%====================================================================

init(Args) ->
  Writer = make_writer(),
  Format = proplists:get_value(format, Args, "{time} - {message}"),
  TimeFormat = proplists:get_value(time_format, Args, "%F %T.%L"),
  Formatter = make_formatter(Format, TimeFormat),
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
      NewState = display(Fmt, Args, Opts, State),
      {ok, NewState};
    _ ->
      {ok, State}
  end.

handle_info({'DOWN', Ref, process, Pid, normal}, #ctx{queue=Q} = State) ->
  case dict:find(Ref, Q) of
    {ok, Pid} ->
      {ok, State#ctx{queue=dict:erase(Ref, Q)}};
    error ->
      {ok, State}
  end.

terminate(_Args, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

make_writer() ->
  fun(Msg) ->
    io:format(standard_error, "~s~n", [Msg])
  end.

make_formatter(Format, TimeFormat) ->
  {ok, Fmt, Keys} = make_format(Format),
  fun(Opts) ->
    Args = lists:map(fun
      (K) when K == time -> strftime:f(get_value(K, Opts), TimeFormat);
      (K) -> get_value(K, Opts)
    end, Keys),
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

get_value(Key, Opts) ->
  case lists:keyfind(Key, 1, Opts) of
    {Key, Value} -> Value;
    false -> undefined
  end.

display(Fmt, Args, Opts0, State) ->
  #ctx{formatter=Formatter, writer=Writer, queue=Q} = State,
  {Pid, Ref} = spawn_monitor(fun() ->
    Opts = [{message, io_lib:format(Fmt, Args)}, {time, os:timestamp()}|Opts0],
    Msg = Formatter(Opts),
    Writer(Msg)
  end),
  State#ctx{queue=dict:store(Ref, Pid, Q)}.

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
