-module(aux_writer_stderr).

-export([make/1]).

make([]) ->
  fun(Msg) ->
    io:format(standard_error, "~s~n", [Msg])
  end.