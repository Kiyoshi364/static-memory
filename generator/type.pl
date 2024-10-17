:- module(type, [
  type/3
]).

type(text, F, Arg) :- !, arg(Arg, F, Val), type_(literal, Val, F-Arg).
type(date, F, Arg) :- !, arg(Arg, F, Val), type_(year_month, Val, F-Arg).
type(link, F, Arg) :- !, arg(Arg, F, Val), type_(name_link, Val, F-Arg).
type(T, F, Arg) :- arg(Arg, F, Val), throw(unknown_type_while_checking(T, Val)).

type_(_, to_be_filled, _) :- !.
type_(literal, Val, Ctx) :-
  ( Val = literal(L) -> check_(atom, L, Ctx)
  ; check_error(literal, Val, Ctx)
  ).
type_(year_month, Val, Ctx) :-
  ( Val = year_month(Y, M) ->
    check_(integer, Y, Ctx),
    check_(integer, M, Ctx)
  ; check_error(year_month, Val, Ctx)
  ).
type_(name_link, Val, Ctx) :-
  ( Val = name_link(N, L) ->
    check_(atom, N, Ctx),
    type_(link_target, L, Ctx)
  ; Val = doi(ID) ->
    check_(atom, ID, Ctx)
  ; check_error(name_link, Val, Ctx)
  ).
type_(link_target, Val, Ctx) :-
  ( Val = publications(Path) -> check_(atom, Path, Ctx)
  ; Val = https(Path)        -> check_(atom, Path, Ctx)
  ; Val = http(Path)         -> check_(atom, Path, Ctx)
  ; check_error(link_target, Val, Ctx)
  ).

:- meta_predicate(check_(1, ?, ?, ?)).
check_(Pred, Val, Ctx) :- ( call(Pred, Val) -> true ; check_error(Pred, Val, Ctx) ).

check_error(Expected, Found, F-Arg) :-
  throw(error(expected_found_functor_colunm(Expected, Found, F, Arg))).
