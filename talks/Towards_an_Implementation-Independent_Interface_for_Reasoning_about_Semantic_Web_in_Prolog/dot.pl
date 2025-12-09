:- module(dot, [
  edge//2, edge//3,
  attr//1, attrs//1,
  attr_key//1, attr_val//1,
  same_rank//2
]).

:- use_module(library(dcgs), []).
:- use_module(library(lists), [foldl/4]).

:- meta_predicate(edge(0, 0, ?, ?)).
:- meta_predicate(edge(0, 0, ?, ?, ?)).

edge(Na, Nb) --> edge(Na, Nb, []).
edge(Na, Nb, Attrs) --> call(Na), " -> ", call(Nb), attrs(Attrs), ";\n".

attrs([]) --> [].
attrs([A | Attrs]) -->
  "[", attr(A), foldl(attrs_, Attrs), "]".

attrs_(A) --> ",", attr(A).

attr(K=V) --> attr_key(K), attr_val(V).

attr_key(K) --> format_("~a=", [K]).

attr_val([]) --> "\"\"".
attr_val([H|T]) --> format_("\"~s\"", [[H|T]]).
attr_val(X) --> { atom(X) }, format_("~a", [X]).

same_rank(M, [N0 | Ls]) -->
  "{ rank=same; ", call(M:N0), foldl(same_rank_(M), Ls), "; }\n".

same_rank_(M, N) --> "; ", call(M:N).
