-module(aux_formatter_string).

-behaviour(aux_formatter).

-export([make/1, accepted_types/0]).

%%====================================================================
%% Callback functions
%%====================================================================

make(Cfg) ->
  Format = proplists:get_value(format, Cfg, "{time} - {message}"),
  TimeFormat = proplists:get_value(time_format, Cfg, "%F %T.%L"),
  {ok, Fmt, Keys} = make_format(Format),
  fun(Opts) ->
    Args = lists:map(fun
      (K) when K == time -> strftime:f(get_value(K, Opts), TimeFormat);
      (K) -> get_value(K, Opts)
    end, Keys),
    io_lib:format(Fmt, Args)
  end.

accepted_types() ->
  ['message'].

%%====================================================================
%% Internal functions
%%====================================================================

make_format(Format) ->
  {ok, Tokens, _} = erl_scan:string(Format, 1, [return_white_spaces]),
  make_format(Tokens, {[], [], false}).

make_format([], {Fmt, Keys, _}) ->
  {ok, lists:concat(lists:reverse(Fmt)), lists:reverse(Keys)};
make_format([Token | Rest], {Fmt, Keys, WriteFlag}) ->
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
          make_format(Rest, {[map_key(Char) | Fmt], [Char | Keys], true})
      end;
    Char ->
      make_format(Rest, {[Char | Fmt], Keys, false})
  end.

get_value(Key, Opts) ->
  case lists:keyfind(Key, 1, Opts) of
    {Key, Value} -> Value;
    false -> undefined
  end.

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
