-module(aux_formatter).

-callback make(Args :: list()) -> Fun :: fun().
-callback accepted_types() -> Types :: list() | '_'.

-export([make_callback/1, make_filter/1]).

%%====================================================================
%% API functions
%%====================================================================

make_callback(Args) ->
  {formatter, Cfg} = lists:keyfind(formatter, 1, Args),
  {module, Mod} = lists:keyfind(module, 1, Cfg),
  ModArgs = proplists:get_value(args, Cfg, []),
  {ok, erlang:apply(Mod, make, [ModArgs])}.

make_filter(Args) ->
  TypeFilter = type_filter(Args),
  LevelFilter = level_filter(Args),
  {ok, fun({Type, Level}) ->
    TypeFilter(Type) andalso LevelFilter(Level)
  end}.

%%====================================================================
%% Internal functions
%%====================================================================

type_filter(Args) ->
  {formatter, Cfg} = lists:keyfind(formatter, 1, Args),
  {module, Mod} = lists:keyfind(module, 1, Cfg),
  AcceptedTypes = erlang:apply(Mod, accepted_types, []),
  make_general_filter(AcceptedTypes).

level_filter(Args) ->
  {formatter, Cfg} = lists:keyfind(formatter, 1, Args),
  Levels = proplists:get_value(levels, Cfg, '_'),
  make_general_filter(Levels).

make_general_filter('_') ->
  fun(_) -> true end;
make_general_filter(Elements) ->
  fun(Element) -> lists:member(Element, Elements) end.
