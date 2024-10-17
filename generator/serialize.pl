:- module(serialize, [
  serialize_header/2, serialize_body/2
]).

:- use_module(library(lists), [maplist/2]).

serialize_header(S, H) :-
  serialize_header_names(S, H), serialize_header_align(S, H).

serialize_header_names(S, H) :-
  maplist(serialize_header_names_(S), H), write(S, '|\n').

serialize_header_names_(S, Name) :- write(S, '|'), write(S, Name).

serialize_header_align(S, H) :-
  maplist(serialize_header_align_(S), H), write(S, '|\n').

serialize_header_align_(S, _) :- write(S, '|:---:').

serialize_body(S, Func) :-
  functor(Func, _, Arity),
  serialize_body_(S, 0, Arity, Func),
  write(S, '|\n'),
true.

serialize_body_(S, N, Arity, Func) :-
  ( N < Arity ->
    N1 is N+1, arg(N1, Func, Val),
    write(S, '|'), serialize_val(S, Val),
    serialize_body_(S, N1, Arity, Func)
  ; N == Arity
  ).

serialize_val(S, literal(L)) :- write(S, L).
serialize_val(S, year_month(Y, M)) :- write(S, Y), write(S, '-'), serialize_month(S, M).
serialize_val(S, name_link(N, L)) :- write(S, [N]), write(S, '('), serialize_link(S, L), write(S, ')').
serialize_val(S, doi(ID)) :- write(S, ['DOI']), write(S, '('), serialize_link(S, doi(ID)), write(S, ')').
serialize_val(S, to_be_filled) :- write(S, '???').

serialize_month(S, M) :- ( M < 10 -> write(S, 0) ), write(S, M).

serialize_link(S, publications(L)) :- write(S, './publications/'), write(S, L).
serialize_link(S, https(L)) :- write(S, 'https://'), write(S, L).
serialize_link(S, http(L)) :- write(S, 'http://'), write(S, L).
serialize_link(S, doi(ID)) :- write(S, 'https://doi.org/'), write(S, ID).
