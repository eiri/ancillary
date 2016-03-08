-module(aux_writer).

-callback make(Args :: list()) -> Fun :: fun().

-export([make_callback/1]).

make_callback(Args) ->
  {writer, Cfg} = lists:keyfind(writer, 1, Args),
  {module, Mod} = lists:keyfind(module, 1, Cfg),
  ModArgs = proplists:get_value(args, Cfg, []),
  {ok, erlang:apply(Mod, make, [ModArgs])}.
