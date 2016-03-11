-module(aux_writer_file).

-behaviour(aux_writer).

-export([make/1]).

%%====================================================================
%% Callback functions
%%====================================================================

make(Cfg) ->
  {file, FilePath} = lists:keyfind(file, 1, Cfg),
  fun(Msg) ->
    {ok, IoDev} = file:open(FilePath, [append, raw]),
    ok = file:write(IoDev, io_lib:format("~s~n", [Msg])),
    ok = file:close(IoDev)
  end.
