:- module(serialize, [
  link_normalized/3, proglang_normalized/3,
  serialize_number//1, serialize_month//1,
  foldlf/5,
  foldlfn/6
]).

:- use_module(library(lists), [append/3]).
:- use_module(library(dcgs), [seq//1]).

:- use_module('..'/proglangs, [proglang_val/2]).
:- use_module('..'/me, [mygithub/1, mygitlab/1]).

link_normalized(Val, Text, Link) :-
  ( Val = text_link(N, L) -> Text = N, linktarget_link(L, Link)
  ; Val = doi(ID)         -> append("DOI(", S0, Text), append(ID, ")", S0), linktarget_link(Val, Link)
  ; Val = mygithub(Path)  -> mygithub(GITHUB), append(GITHUB, [(/) | Path], Text), linktarget_link(Val, Link)
  ; Val = mygitlab(Path)  -> mygitlab(GITLAB), append(GITLAB, [(/) | Path], Text), linktarget_link(Val, Link)
  ; throw(unknown_link_while_normalizing(Val))
  ).

linktarget_link(publications(L), publications(L)) :- !.
linktarget_link(https(L), external(Iri)) :- !, append("https://", L, Iri).
linktarget_link(http(L), external(Iri)) :- !, append("http://", L, Iri).
linktarget_link(doi(ID), external(Iri)) :- !, append("https://doi.org/", ID, Iri).
linktarget_link(mygithub(Path), external(Iri)) :- !, mygithub(GITHUB), append("https://", S0, Iri), append(GITHUB, ['/' | Path], S0).
linktarget_link(mygitlab(Path), external(Iri)) :- !, mygitlab(GITLAB), append("https://", S0, Iri), append(GITLAB, ['/' | Path], S0).
linktarget_link(Link, _) :-
  throw(unknown_linktarget_while_normalizing(Link)).

proglang_normalized(PL, Text, Link) :- proglang_val(PL, Val), link_normalized(Val, Text, Link).

serialize_number(N) --> { number_chars(N, Cs) }, seq(Cs).
serialize_month(M) --> ( { M < 10 } -> "0" ; [] ), serialize_number(M).

:- meta_predicate(foldlf(4, ?, ?, ?, ?)).

foldlf(G_4, L, F, X0, X) :-
  functor(F, _, Arity),
  foldlf_(L, G_4, 0, Arity, F, X0, X).

foldlf_([], _, Arity, Arity, _, X, X).
foldlf_([A | L], G_4, N, Arity, F, X0, X) :-
  N < Arity,
  N1 is N + 1, arg(N1, F, Val),
  call(G_4, A, Val, X0, X1),
  foldlf_(L, G_4, N1, Arity, F, X1, X).

:- meta_predicate(foldlfn(6, ?, ?, ?, ?, ?)).

foldlfn(G_6, L0, L, F, X0, X) :-
  functor(F, _, Arity),
  foldlfn_(L0, L, G_6, 0, Arity, F, X0, X).

foldlfn_([], [], _, Arity, Arity, _, X, X).
foldlfn_([A0 | L0], [A | L], G_6, N, Arity, F, X0, X) :-
  N < Arity,
  N1 is N + 1, arg(N1, F, Val),
  call(G_6, A0, A, Val, N1, X0, X1),
  foldlfn_(L0, L, G_6, N1, Arity, F, X1, X).
