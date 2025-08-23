:- module(type, [
  type/2, check_field/3,
  string/1
]).

:- use_module(library(lists), [member/2]).

:- use_module(proglangs, [proglang/1]).

check_field(field(N, T), F, Val) :- type(T, field(N)), type_(T, Val, F).

type(text, _) :- !.
type(date, _) :- !.
type(link, _) :- !.
type(proglang, _) :- !.
type(list(T, _, _, _), Ctx) :- !, type(T, list(Ctx)).
type(or(Ts), Ctx) :- !, typeor(Ts, 0, Ctx).
type(T, Ctx) :- throw(unknown_type_while_checking(T, Ctx)).

typeor([], _, _).
typeor([T | Ts], N, Ctx) :-
  type(T, or(N, Ctx)),
  N1 is N + 1,
  typeor(Ts, N1, Ctx).

type_(_, to_be_filled, _) :- !.
type_(text, Val, Ctx) :-
  ( Val = literal(L) -> check_(string, L, Ctx)
  ; check_error(literal, Val, Ctx)
  ).
type_(date, Val, Ctx) :-
  ( Val = year_month(Y, M) ->
    check_(integer, Y, Ctx),
    check_(integer, M, Ctx)
  ; check_error(year_month, Val, Ctx)
  ).
type_(link, Val, Ctx) :-
  ( Val = text_link(_, _) -> type_(text_link, Val, Ctx)
  ; Val = doi(ID)         -> check_(string, ID, Ctx)
  ; Val = mygithub(Path)  -> check_(string, Path, Ctx)
  ; Val = mygitlab(Path)  -> check_(string, Path, Ctx)
  ; check_error(link, Val, Ctx)
  ).
type_(text_link, Val, Ctx) :-
  ( Val = text_link(N, L) ->
    check_(string, N, Ctx),
    type_(link_target, L, Ctx)
  ; check_error(text_link, Val, Ctx)
  ).
type_(link_target, Val, Ctx) :-
  ( Val = publications(Path) -> check_(string, Path, Ctx)
  ; Val = https(Path)        -> check_(string, Path, Ctx)
  ; Val = http(Path)         -> check_(string, Path, Ctx)
  ; Val = doi(Path)          -> check_(string, Path, Ctx)
  ; Val = mygithub(Path)     -> check_(string, Path, Ctx)
  ; Val = mygitlab(Path)     -> check_(string, Path, Ctx)
  ; check_error(link_target, Val, Ctx)
  ).
type_(proglang, Val, Ctx) :-
  ( Val = proglang(PL) -> check_(atom, PL, Ctx), check_(proglang, PL, Ctx)
  ; check_error(proglang, Val, Ctx)
  ).
type_(list(T, J, E, N), Val, Ctx) :- type_list(T, J, E, N, Val, Ctx).
type_(or(Ts), Val, Ctx) :- type_or(Ts, Val, Ctx).

type_list(Type, Join, End, None, List, Ctx) :-
  ( List = [H | _] -> check_type(Type, H, List, Ctx) ; true ),
  check_(string, Join, Ctx),
  check_(string, End, Ctx),
  check_(string, None, Ctx),
  type_list_(List, Type, Ctx).

type_list_([], _, _).
type_list_([Val | Vals], Type, Ctx) :-
  type_(Type, Val, Ctx), type_list_(Vals, Type, Ctx).

type_or(Ts, Val, Ctx) :-
  ( Val = or(T, V), member(T, Ts) -> type_(T, V, Ctx)
  ; check_error(or(Ts), Val, Ctx)
  ).

string(V) :-
  nonvar(V),
  ( V = [] -> true
  ; V = [H | T] -> atom(H), atom_length(H, 1), string(T)
  ).

:- meta_predicate(check_(1, ?, ?, ?)).
check_(Pred, Val, Ctx) :- ( call(Pred, Val) -> true ; check_error(Pred, Val, Ctx) ).

check_type(T, Val, ErrVal, Ctx) :-
  ( type_(T, Val, Ctx) -> true
  ; throw(unknown_type_while_checking(T, ErrVal))
  ).

check_error(Expected, Found, F-Arg) :-
  throw(error(expected_found_functor_colunm(Expected, Found, F, Arg))).
