:- module(serialize, [
  serialize_number//1, serialize_month//1
]).

:- use_module(library(dcgs), [seq//1]).

serialize_number(N) --> { number_chars(N, Cs) }, seq(Cs).
serialize_month(M) --> ( { M < 10 } -> "0" ; [] ), serialize_number(M).
