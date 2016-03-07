-module(aux_formatter).

-callback make(Args :: list()) -> Fun :: fun().
