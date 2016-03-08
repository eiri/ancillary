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
  SeverityFilter = severity_filter(Args),
  {ok, fun({Type, Severity}) ->
    TypeFilter(Type) andalso SeverityFilter(Severity)
  end}.

%%====================================================================
%% Internal functions
%%====================================================================

type_filter(Args) ->
  {formatter, Cfg} = lists:keyfind(formatter, 1, Args),
  {module, Mod} = lists:keyfind(module, 1, Cfg),
  AcceptedTypes = erlang:apply(Mod, accepted_types, []),
  make_general_filter(AcceptedTypes).

severity_filter(Args) ->
  {formatter, Cfg} = lists:keyfind(formatter, 1, Args),
  Severities = proplists:get_value(severities, Cfg, '_'),
  make_general_filter(Severities).

make_general_filter('_') ->
  fun(_) -> true end;
make_general_filter(Elements) ->
  fun(Element) -> lists:member(Element, Elements) end.
