-module(aux_formatter_json).

-behaviour(aux_formatter).

-export([make/1, accepted_types/0]).

%%====================================================================
%% Callback functions
%%====================================================================

make(Cfg) ->
  Keys = proplists:get_value(keys, Cfg, '_'),
  TimeFormat = proplists:get_value(time_format, Cfg, "%F %T.%L"),
  Normalizer = make_normalizer(Keys, TimeFormat),
  fun(Opts) ->
    Data = Normalizer(Opts),
    jiffy:encode(Data)
  end.

accepted_types() ->
  [message].

%%====================================================================
%% Internal functions
%%====================================================================

make_normalizer('_', TimeFormat) ->
  Keys = lists:flatmap(fun(Type) ->
    aux_formatter:get_keys(Type)
  end, accepted_types()),
  make_normalizer(Keys, TimeFormat);
make_normalizer(Keys, TimeFormat) ->
  fun(Opts) ->
    Data = lists:filtermap(fun({Key, Value}) ->
      case lists:member(Key, Keys) of
        true when Key == time ->
          {true, {time, iolist_to_binary(strftime:f(Value, TimeFormat))}};
        true ->
          {true, normalize(Key, Value)};
        false ->
          false
      end
    end, Opts),
    {Data}
  end.

normalize(Key, Value) when is_atom(Value) ->
  {Key, Value};
normalize(Key, Value) when is_pid(Value) ->
  {Key, iolist_to_binary(io_lib:format("~p", [Value]))};
normalize(Key, Value) ->
  {Key, iolist_to_binary(Value)}.
