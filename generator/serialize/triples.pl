:- module(triples,
[ triples_predicates//6
, check_triplification/4
]).

:- use_module(library(lists),
[ member/2, append/3, foldl/4, maplist/3
]).
:- use_module(library(dcgs), [phrase/3]).

:- use_module('..'/type, [string/1]).

:- use_module(text, [lowercase/2]).
:- use_module(serialize,
[ link_normalized/3, proglang_normalized/3
, serialize_number//1, serialize_month//1
, foldlfn/6
]).

triples_predicates(Fs, Ps, Me, SubN, SubEx, Func) -->
  triple(Me, foaf:made, Sub),
  foldlfn(triples_predicates_(SubN, SubEx, Sub), Fs, Ps, Func).

triples_predicates_(SubN, SubEx, Sub, field(Name, T), P, Val, N1) -->
  { type_val_resource(T, Val, Obj),
    ( SubN == N1 -> extract_subject(SubEx, Obj, Sub) ; true ),
    type_fieldname_predicates(T, Name, P, P1)
  },
  triple_predicate(P1, Sub, Obj).

triple_predicate([], _, _) --> !.
triple_predicate([P | Ps], Sub, Obj) --> !, triple_predicate(P, Sub, Obj), triple_predicate(Ps, Sub, Obj).
triple_predicate(:(P), Sub, Obj) --> !, triple(Sub, :(P), Obj).
triple_predicate(N:P, Sub, Obj) --> !, triple(Sub, N:P, Obj).
triple_predicate(link(Tag, P), Sub, Obj) --> !, triple_predicate_link(Tag, P, Sub, Obj).
triple_predicate(list_each(P), Sub, Objs) --> !, foldl(triple_predicate(P, Sub), Objs).
triple_predicate(or(Ps), Sub, Obj) --> !, triple_predicate_or(Obj, Ps, Sub).
triple_predicate(Pred, Sub, Obj) -->
  { throw(unknown_triple_function_while_converting_to_triple(Pred, Sub, Obj)) }.

triple_predicate_link(Tag, P, Sub, link(Text, Ref)) -->
  { ( Tag = text -> Obj = Text
    ; Tag = ref -> Obj = Ref
    ; throw(unknown_link_tag_while_converting_to_triple(Tag, P, Sub, link(Text, Ref)))
  ) },
  triple_predicate(P, Sub, Obj).

triple_predicate_or(O, Ps, Sub) -->
  ( { O = or(T, Obj), member(T-P, Ps) } -> triple_predicate(P, Sub, Obj)
  ; { throw(unknown_or_tag_while_converting_to_triple(O, Ps, Sub)) }
  ).

triple(Sub, Pred, Obj) --> [t(Sub, Pred, Obj)].

type_fieldname_predicates(text, N, Ps0, [:(N) | Ps0]) :- !.
type_fieldname_predicates(date, N, Ps0, [:(N) | Ps0]) :- !.
type_fieldname_predicates(link, N, Ps0, [link(text, :(NT)), link(ref, :(NR)) | Ps0]) :- !, atom_concat(N, '_name', NT), atom_concat(N, '_link', NR).
type_fieldname_predicates(proglang, N, Ps0, [link(text, :(NT)), link(ref, :(NR)) | Ps0]) :- !, atom_concat(N, '_name', NT), atom_concat(N, '_link', NR).
type_fieldname_predicates(list(T, _, _, _), N, Ps0, [list_each(P) | Ps0]) :- !, type_fieldname_predicates(T, N, [], P).
type_fieldname_predicates(or(Ts), N, Ps0, [or(P) | Ps0]) :- !, type_fieldname_predicates_or(Ts, N, P).
type_fieldname_predicates(T, N, Ps0, _) :-
  throw(unknown_type_fieldname_while_adding_default_predicate(T, N, Ps0)).

type_fieldname_predicates_or([], _, []).
type_fieldname_predicates_or([T | Ts], N, [T-P | Ps]) :-
  type_fieldname_predicates(T, N, [], P),
  type_fieldname_predicates_or(Ts, N, Ps).

type_val_resource(text, literal(L), literal(xsd:string, L)) :- !.
type_val_resource(date, year_month(Y, M), literal(xsd:gYearMonth, S)) :- !, format_year_month(Y, M, S).
type_val_resource(link, Val, link(literal(xsd:string, Text), Ref)) :- !, link_normalized(Val, Text, Link), linktarget_resource(Link, Ref).
type_val_resource(proglang, proglang(PL), link(literal(xsd:string, Text), Ref)) :- !, proglang_normalized(PL, Text, Link), linktarget_resource(Link, Ref).
type_val_resource(list(T, _, _, _), L, Res) :- !, maplist(type_val_resource(T), L, Res).
type_val_resource(or(Ts), O, Res) :- !, or_resource(Ts, O, Res).
type_val_resource(T, Val, Res) :-
  throw(unknown_type_val_while_converting_to_resource(T, Val, Res)).

format_year_month(Y, M, S) :-
  Body = ( serialize_number(Y), "-", serialize_month(M) ),
  phrase(Body, S, []).

linktarget_resource(publications(L), :(A)) :- !, append("publications/", L, Iri), atom_chars(A, Iri).
linktarget_resource(external(L), iri(L)) :- !.
linktarget_resource(Link, _) :- !,
  throw(unknown_linktarget_while_converting_to_resource(Link)).

or_resource(Ts, O, or(T, Res)) :-
  ( O = or(T, Val), member(T, Ts) -> type_val_resource(T, Val, Res)
  ; throw(unknown_or_tag_while_converting_to_resource(Ts, Val))
  ).

extract_subject([Ex | Exs], Obj, Sub) :-
  ( Obj = literal(xsd:string, Str) -> extract_subject_text(Ex, Exs, Str, Sub)
  ; throw(unreachable(extract_subject(local, Obj, Sub)))
  ).
extract_subject(text(Ex), Obj, Sub) :-
  ( Obj = link(Text, _) -> extract_subject(Ex, Text, Sub)
  ; throw(unreachable(extract_subject(text, Obj, Sub)))
  ).
extract_subject(ref, Obj, Sub) :-
  ( Obj = link(_, Sub) -> true
  ; throw(unreachable(extract_subject(ref, Obj, Sub)))
  ).
extract_subject(or(Exs), O, Sub) :-
  ( O = or(Tag, Obj), member(Tag-Ex, Exs) -> extract_subject(Ex, Obj, Sub)
  ; throw(unreachable(extract_subject(or(Exs), Obj, Sub)))
  ).

extract_subject_text(local, _, Str, :(A)) :- atom_chars(A, Str).
extract_subject_text(prefixed(N), _, Str, N:A) :- atom_chars(A, Str).
extract_subject_text(lowercase, [Ex | Exs], Str0, Sub) :-
  lowercase(Str0, Str),
  extract_subject_text(Ex, Exs, Str, Sub).
extract_subject_text(prepend(A), [Ex | Exs], Str0, Sub) :-
  append(A, Str0, Str),
  extract_subject_text(Ex, Exs, Str, Sub).
extract_subject_text(pospend(A), [Ex | Exs], Str0, Sub) :-
  append(Str0, A, Str),
  extract_subject_text(Ex, Exs, Str, Sub).

check_triplification(Ts, SubN, SubEx, Ps) :- check_triplification_(Ts, Ps, 1, SubN, SubEx).

check_triplification_([], [], _, _, _).
check_triplification_([field(_, T) | Ts], [P | Ps], N, SubN, SubEx) :-
  N1 is N + 1,
  ( N == SubN -> check_extract(T, SubEx, SubN)
  ; true
  ),
  check_predicate(T, P, T-P),
  check_triplification_(Ts, Ps, N1, SubN, SubEx).

check_predicate(text, P, Ctx) :- !, check_predicate_simple(P, Ctx).
check_predicate(date, P, Ctx) :- !, check_predicate_simple(P, Ctx).
check_predicate(link, P, Ctx) :- !, check_predicate_link(P, Ctx).
check_predicate(proglang, P, Ctx) :- !, check_predicate_link(P, Ctx).
check_predicate(list(T, _, _, _), P, Ctx) :- !, check_predicate_list(T, P, Ctx).
check_predicate(or(Ts), P, Ctx) :- !, check_predicate_or(Ts, P, Ctx).
check_predicate(T, P, Ctx) :- throw(unknown_type_while_checking_triplification(T, P, Ctx)).

check_predicate_simple([], _) :- !.
check_predicate_simple([P | Ps], Ctx) :- !, check_predicate_simple(P, Ctx), check_predicate_simple(Ps, Ctx).
check_predicate_simple(:(P), Ctx) :- !, check_(atom, P, Ctx).
check_predicate_simple(N:P, Ctx) :- !, check_(atom, N, Ctx), check_(atom, P, Ctx).
check_predicate_simple(P, Ctx) :-
  check_error(simple_predicate, P, Ctx).

check_predicate_link(PL, Ctx) :-
  ( PL = link(Tag, P) ->
    ( Tag = text -> true
    ; Tag = ref -> true
    ; check_error(link_tag, Tag, Ctx)
    ),
    check_predicate_simple(P, Ctx)
  ; PL = [] -> true
  ; PL = [A | B] ->
    ( check_predicate_link(A, Ctx),
      check_predicate_link(B, Ctx)
    )
  ; check_error(link_predicate, PL, Ctx)
  ).

check_predicate_list(T, PL, Ctx) :-
  ( PL = list_each(P) ->
    check_predicate(T, P, Ctx)
  ; check_error(list_predicate, PL, Ctx)
  ).

check_predicate_or(Ts, PO, Ctx) :-
  ( PO = or(Ps) ->
    check_predicate_or_(Ts, Ps, Ctx)
  ; check_error(or_predicate, PO, Ctx)
  ).

check_predicate_or_([], [], _).
check_predicate_or_([T | Ts], Ps0, Ctx) :-
  ( Ps0 = [P0 | Ps] ->
    ( P0 = T-P ->
      check_predicate(T, P, ctx_inside(T-P, Ctx))
    ; check_error(or_case(T), P0, Ctx)
    )
  ; check_error(or_case_list(T), Ps0, Ctx)
  ),
  check_predicate_or_(Ts, Ps, Ctx).

check_extract(T, Ex, SubN) :-
  ( check_extract_(T, Ex, SubN) -> true
  ; throw(invalid_extraction_while_checking_triplification(T, Ex, SubN))
  ).

check_extract_(text, Ex, SubN) :- !, check_extract_text(Ex, Ex-SubN).
check_extract_(date, _, _) :- !, false.
check_extract_(link, Ex, SubN) :- !,
  ( Ex = text(E) -> check_extract_text(E, extract_link(link, Ex, SubN))
  ; Ex = ref  -> true
  ; throw(unknown_extraction_while_checking_triplification(link, Ex, SubN))
  ).
check_extract_(proglang, Ex, SubN) :- !, check_extract_(link, Ex, SubN).
check_extract_(list(_, _, _, _), _, _) :- !, false.
check_extract_(or(Ts), Ex, SubN) :- !, check_extract_or(Ts, Ex, SubN).
check_extract_(T, Ex, SubN) :- throw(unknown_type_while_checking_extraction(T, Ex, SubN)).

check_extract_text([], Ctx) :- throw(text_extraction_unfinished_while_checking_triplification(Ctx)).
check_extract_text([Ex | Exs], Ctx) :-
  ( Ex = local -> check_extract_text_finished(Exs, Ctx)
  ; Ex = prefixed(N) -> check_(atom, N, Ctx), check_extract_text_finished(Exs, Ctx)
  ; Ex = lowercase -> check_extract_text(Exs, Ctx)
  ; Ex = prepend(A) -> check_(string, A, Ctx), check_extract_text(Exs, Ctx)
  ; Ex = pospend(A) -> check_(string, A, Ctx), check_extract_text(Exs, Ctx)
  ; throw(unknown_text_extraction_while_checking_triplification(Ex, Exs, Ctx))
  ).

check_extract_text_finished(Exs, Ctx) :-
  ( Exs = [] -> true ; throw(text_extraction_finished_early(Exs, Ctx)) ).

check_extract_or(Ts, Ex, SubN) :-
  ( Ex = or(Exs) -> true
  ; check_error(or_case_list, Ex, SubN)
  ),
  check_extract_or_(Ts, Exs, extract_or(Ts, Ex, SubN)).

check_extract_or_([], [], _).
check_extract_or_([T | Ts], Exs0, Ctx) :-
  ( Exs0 = [Ex0 | Exs] ->
    ( Ex0 = T-Ex ->
      ( check_extract_(T, Ex, Ctx)
      ; throw(invalid_extraction_inside_or_while_checking_triplification(T, Ex, Ctx))
      )
    ; check_error(or_case(T), Ex0, Ctx)
    )
  ; check_error(or_case_list(T), Exs0, Ctx)
  ),
  check_extract_or_(Ts, Exs, Ctx).

check_(Pred, Val, Ctx) :- ( call(Pred, Val) -> true ; check_error(Pred, Val, Ctx) ).

check_error(Expected, Found, Ctx) :-
  throw(error(expected_found_where(Expected, Found, Ctx))).
