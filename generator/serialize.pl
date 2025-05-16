:- module(serialize, [
  serialize_header/2, serialize_body/3
]).

:- use_module(library(lists), [member/2, maplist/2]).

:- use_module(proglangs, [proglang_val/2]).
:- use_module(me, [mygithub/1, mygitlab/1]).

serialize_header(S, H) :-
  serialize_header_names(S, H), serialize_header_align(S, H).

serialize_header_names(S, H) :-
  maplist(serialize_header_names_(S), H), write(S, '|\n').

serialize_header_names_(S, Name) :- write(S, '|'), write(S, Name).

serialize_header_align(S, H) :-
  maplist(serialize_header_align_(S), H), write(S, '|\n').

serialize_header_align_(S, _) :- write(S, '|:---:').

serialize_body(S, Ts, Func) :-
  functor(Func, _, Arity),
  serialize_body_(Ts, S, 0, Arity, Func),
  write(S, '|\n'),
true.

serialize_body_([], _, Arity, Arity, _).
serialize_body_([T | Ts], S, N, Arity, Func) :-
  N < Arity,
  N1 is N+1, arg(N1, Func, Val),
  write(S, '|'), serialize_type_val(T, Val, S),
  serialize_body_(Ts, S, N1, Arity, Func).

serialize_type_val(text, literal(L), S) :- !, write(S, L).
serialize_type_val(date, year_month(Y, M), S) :- !, write(S, Y), write(S, '-'), serialize_month(M, S).
serialize_type_val(link, Val, S) :- !,
  ( Val = name_link(N, L) -> write(S, [N]), write(S, '('), serialize_linktarget(L, S), write(S, ')')
  ; Val = doi(ID)         -> write(S, ['DOI']), write(S, '('), serialize_linktarget(doi(ID), S), write(S, ')')
  ; Val = mygithub(Path)  -> mygithub(GITHUB), write(S, '['), write(S, GITHUB), write(S, /), write(S, Path), write(S, ']('), serialize_linktarget(mygithub(Path), S), write(S, ')')
  ; Val = mygitlab(Path)  -> mygitlab(GITLAB), write(S, '['), write(S, GITLAB), write(S, /), write(S, Path), write(S, ']('), serialize_linktarget(mygitlab(Path), S), write(S, ')')
  ; throw(unknown_link_while_serializing(Val))
  ).
serialize_type_val(proglang, proglang(PL), S) :- !, proglang_val(PL, Val), serialize_type_val(link, Val, S).
serialize_type_val(list(T, J, E, N), L, S) :- !, serialize_list(L, T, J, E, N, S).
serialize_type_val(or(Ts), O, S) :- !, serialize_or(Ts, O, S).
serialize_type_val(_, to_be_filled, S) :- !, write(S, '???').
serialize_type_val(T, Val, _) :-
  throw(unknown_type_val_while_serializing(T, Val)).

serialize_month(M, S) :- ( M < 10 -> write(S, 0) ), write(S, M).

serialize_linktarget(publications(L), S) :- !, write(S, './publications/'), write(S, L).
serialize_linktarget(https(L), S) :- !, write(S, 'https://'), write(S, L).
serialize_linktarget(http(L), S) :- !, write(S, 'http://'), write(S, L).
serialize_linktarget(doi(ID), S) :- !, write(S, 'https://doi.org/'), write(S, ID).
serialize_linktarget(mygithub(Path), S) :- !, mygithub(GITHUB), write(S, 'https://'), write(S, GITHUB), write(S, /), write(S, Path).
serialize_linktarget(mygitlab(Path), S) :- !, mygitlab(GITLAB), write(S, 'https://'), write(S, GITLAB), write(S, /), write(S, Path).
serialize_linktarget(Link, _) :-
  throw(unknown_link_while_serializing(Link)).

serialize_list([], _, _, _, None, S) :- write(S, None).
serialize_list([Val0 | Vals], T, Join, End, _, S) :- serialize_list_(Vals, Val0, T, Join, End, S).

serialize_list_([], Val0, T, _, End, S) :- serialize_type_val(T, Val0, S), write(S, End).
serialize_list_([Val1 | Vals], Val0, T, Join, End, S) :-
  serialize_type_val(T, Val0, S), write(S, Join), serialize_list(Vals, Val1, T, Join, End, S).

serialize_or(Ts, O, S) :-
  ( O = or(T, Val), member(T, Ts) -> serialize_type_val(T, Val, S)
  ; throw(unknown_or_while_serializing(Ts, Val))
  ).
