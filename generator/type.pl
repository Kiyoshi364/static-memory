:- module(type, [
  type/3
]).

:- use_module(library(lists), [member/2]).

:- use_module(proglangs, [proglang/1]).

type(text, F, Arg) :- !, arg(Arg, F, Val), type_(text, Val, F-Arg).
type(date, F, Arg) :- !, arg(Arg, F, Val), type_(date, Val, F-Arg).
type(link, F, Arg) :- !, arg(Arg, F, Val), type_(link, Val, F-Arg).
type(proglang, F, Arg) :- !, arg(Arg, F, Val), type_(proglang, Val, F-Arg).
type(list(T, J, E, N), F, Arg) :- !, arg(Arg, F, Val), type_list(T, J, E, N, Val, F-Arg).
type(or(Ts), F, Arg) :- !, arg(Arg, F, Val), type_or(Ts, Val, F-Arg).
type(T, F, Arg) :- arg(Arg, F, Val), throw(unknown_type_while_checking(T, Val)).

type_(_, to_be_filled, _) :- !.
type_(text, Val, Ctx) :-
  ( Val = literal(L) -> check_(atom, L, Ctx)
  ; check_error(literal, Val, Ctx)
  ).
type_(date, Val, Ctx) :-
  ( Val = year_month(Y, M) ->
    check_(integer, Y, Ctx),
    check_(integer, M, Ctx)
  ; check_error(year_month, Val, Ctx)
  ).
type_(link, Val, Ctx) :-
  ( Val = name_link(_, _) -> type_(name_link, Val, Ctx)
  ; Val = doi(ID)         -> check_(atom, ID, Ctx)
  ; Val = mygithub(Path)  -> check_(atom, Path, Ctx)
  ; Val = mygitlab(Path)  -> check_(atom, Path, Ctx)
  ; check_error(link, Val, Ctx)
  ).
type_(name_link, Val, Ctx) :-
  ( Val = name_link(N, L) ->
    check_(atom, N, Ctx),
    type_(link_target, L, Ctx)
  ; check_error(name_link, Val, Ctx)
  ).
type_(link_target, Val, Ctx) :-
  ( Val = publications(Path) -> check_(atom, Path, Ctx)
  ; Val = https(Path)        -> check_(atom, Path, Ctx)
  ; Val = http(Path)         -> check_(atom, Path, Ctx)
  ; Val = mygithub(Path)     -> check_(atom, Path, Ctx)
  ; Val = mygitlab(Path)     -> check_(atom, Path, Ctx)
  ; check_error(link_target, Val, Ctx)
  ).
type_(proglang, Val, Ctx) :-
  ( Val = proglang(PL) -> check_(atom, PL, Ctx), proglang(PL)
  ; check_error(proglang, Val, Ctx)
  ).

type_list(Type, Join, End, None, List, Ctx) :-
  check_(atom, Join, Ctx),
  check_(atom, End, Ctx),
  check_(atom, None, Ctx),
  type_list_(List, Type, Ctx).

type_list_([], _, _).
type_list_([Val | Vals], Type, Ctx) :-
  type_(Type, Val, Ctx), type_list_(Vals, Type, Ctx).

type_or(Ts, Val, Ctx) :-
  ( Val = or(T, V), member(T, Ts) -> type_(T, V, Ctx)
  ; check_error(or(Ts), Val, Ctx)
  ).

:- meta_predicate(check_(1, ?, ?, ?)).
check_(Pred, Val, Ctx) :- ( call(Pred, Val) -> true ; check_error(Pred, Val, Ctx) ).

check_error(Expected, Found, F-Arg) :-
  throw(error(expected_found_functor_colunm(Expected, Found, F, Arg))).
