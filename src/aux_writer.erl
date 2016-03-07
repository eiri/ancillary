-module(aux_writer).

-callback make(Args :: list()) -> Fun :: fun().
