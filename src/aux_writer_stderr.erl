-module(aux_writer_stderr).

-behaviour(aux_writer).

-export([make/1]).

%%====================================================================
%% Callback functions
%%====================================================================

make([]) ->
  fun(Msg) ->
    io:format(standard_error, "~s~n", [Msg])
  end.
