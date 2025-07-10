:- module(serialize_ttl, [
  triples_predicates//4,
  serialize_prefixes//0, serialize_prefixes//2,
  serialize_triples//1
]).

:- use_module(library(lists), [
  member/2, append/3, foldl/4, maplist/3
]).
:- use_module(library(dcgs), [phrase/3, seq//1]).
:- use_module(library(reif), [if_/3, (=)/3, memberd_t/3]).

:- use_module(proglangs, [proglang_val/2]).
:- use_module(me, [rdf_me/1, rdf_prefixes/2, mygithub/1, mygitlab/1]).

:- use_module(serialize_md, [serialize_number//1, serialize_month//1]).

triples_predicates(Ts, Ps, SubN, Func) -->
  { functor(Func, _, Arity), rdf_me(Me) },
  [t(Me, foaf:made, Sub)],
  triples_predicates_(Ts, Ps, SubN, Sub, 0, Arity, Func).

triples_predicates_([], [], _, _, Arity, Arity, _) --> [].
triples_predicates_([T | Ts], [P | Ps], SubN, Sub, N, Arity, Func) -->
  { N < Arity,
    N1 is N+1, arg(N1, Func, Val),
    phrase(type_val_resources(T, Val), Objs, []),
    ( SubN == N1 ->
      ( Objs = [Obj] -> Sub = Obj ; throw(error(unreachable(triples_predicates_([T | Ts], [P | Ps], SubN, Sub, N, Arity, Func)))) )
    ; true )
  },
  foldl(triple_predicate(Sub, Objs), P),
  triples_predicates_(Ts, Ps, SubN, Sub, N1, Arity, Func).

triple_predicate(Sub, Objs, Pred) --> foldl(triple_predicate_(Sub, Pred), Objs).

triple_predicate_(Sub, Pred, Obj) --> [t(Sub, Pred, Obj)].

type_val_resources(text, literal(L)) --> !, [literal(xsd:string, L)].
type_val_resources(date, year_month(Y, M)) --> !, { format_year_month(Y, M, S) }, [literal(xsd:gYearMonth, S)].
type_val_resources(link(T), Val) --> !, type_val_resources_link(T, Val).
type_val_resources(proglang, proglang(PL)) --> !, { proglang_val(PL, Val) }, type_val_resources(link(ref), Val).
type_val_resources(listeach(T, _, _, _), L) --> !, foldl(type_val_resources(T), L).
type_val_resources(or(Ts), O) --> !, or_resource(Ts, O).
type_val_resources(T, Val) -->
  { throw(unknown_type_val_while_converting_to_resources(T, Val)) }.

format_year_month(Y, M, S) :-
  Body = ( serialize_number(Y), "-", serialize_month(M) ),
  phrase(Body, S, []).

type_val_resources_link(text, Val) -->
  [literal(xsd:string, Text)],
  { ( Val = name_link(N, _) -> Text = N
    ; Val = doi(ID)         -> append("DOI(", S0, Text), append(ID, ")", S0)
    ; Val = mygithub(Path)  -> mygithub(GITHUB), append(GITHUB, [(/) | Path], Text)
    ; Val = mygitlab(Path)  -> mygitlab(GITLAB), append(GITLAB, [(/) | Path], Text)
    ; throw(unknown_link_while_serializing(Val))
    ) }.
type_val_resources_link(ref, Val) -->
  ( { Val = name_link(_, L) } -> linktarget_resource(L)
  ; { Val = doi(_)          } -> linktarget_resource(Val)
  ; { Val = mygithub(_)     } -> linktarget_resource(Val)
  ; { Val = mygitlab(_)     } -> linktarget_resource(Val)
  ; { throw(unknown_link_while_serializing(Val)) }
  ).

linktarget_resource(publications(L)) --> !, { append("publications/", L, Iri), atom_chars(A, Iri) }, [:(A)].
linktarget_resource(https(L)) --> !, { append("https://", L, Iri) }, [iri(Iri)].
linktarget_resource(http(L)) --> !, { append("http://", L, Iri) }, [iri(Iri)].
linktarget_resource(doi(ID)) --> !, { append("https://doi.org/", ID, Iri) }, [iri(Iri)].
linktarget_resource(mygithub(Path)) --> !, { mygithub(GITHUB), append("https://", S0, Iri), append(GITHUB, ['/' | Path], S0) }, [iri(Iri)].
linktarget_resource(mygitlab(Path)) --> !, { mygitlab(GITLAB), append("https://", S0, Iri), append(GITLAB, ['/' | Path], S0) }, [iri(Iri)].
linktarget_resource(Link, _) -->
  { throw(unknown_link_while_converting_to_resource(Link)) }.

or_resource(Ts, O) -->
  ( { O = or(T, Val), member(T, Ts) } -> type_val_resources(T, Val)
  ; { throw(unknown_link_while_converting_to_resource(Ts, Val)) }
  ).

serialize_prefixes --> { rdf_prefixes(B, Ps) }, serialize_prefixes(B, Ps).
serialize_prefixes(B, Ps) --> foldl(serialize_prefix, Ps), serialize_base_prefix(B).

serialize_prefix(P-L) --> "@prefix ", name(P), ": <", iri(L), "> .\n".
serialize_base_prefix(L) --> "@prefix : <", iri(L), "> .\n".

serialize_triples(Ts0) --> { sort(Ts0, Ts) }, serialize_sorted_triples(Ts).

serialize_sorted_triples([]) --> [].
serialize_sorted_triples([t(S, P, O) | Ts]) --> serialize_sorted_triples_(Ts, start, S, P, O).

serialize_sorted_triples_([], A, S, P, O) --> finish(A, S, P, O).
serialize_sorted_triples_([t(S, P, O) | Ts], A0, S0, P0, O0) -->
  { if_(S = S0,
      if_(P = P0,
        if_(O = O0,
          throw(error(dupplicated_triples(t(S, P, O)))),
          A = sp
        ),
        A = s
      ),
      A = start
    )
  },
  transition(A, A0, S0, P0, O0),
  serialize_sorted_triples_(Ts, A, S, P, O).

transition(start, A0, S0, P0, O0) --> finish(A0, S0, P0, O0), "\n".
transition(s    , A0, S0, P0, O0) --> transition_s(A0, S0, P0, O0).
transition(sp   , A0, S0, P0, O0) --> transition_sp(A0, S0, P0, O0).

transition_s(start, S, P, O) -->
  serialize_resource(S),
  indent(1), serialize_resource(P),
  " ", serialize_resource(O).
transition_s(s, _, P, O) -->
  " ;",
  indent(1), serialize_resource(P),
  " ", serialize_resource(O).
transition_s(sp, _, _, O) -->
  " ,",
  indent(2), serialize_resource(O).

transition_sp(start, S, P, O) -->
  serialize_resource(S),
  indent(1), serialize_resource(P),
  indent(2), serialize_resource(O).
transition_sp(s, _, P, O) -->
  " ;",
  indent(1), serialize_resource(P),
  indent(2), serialize_resource(O).
transition_sp(sp, _, _, O) -->
  " ,", indent(2), serialize_resource(O).

finish(start, S, P, O) -->
  serialize_resource(S), " ",
  serialize_resource(P), " ",
  serialize_resource(O), " .\n".
finish(s, _, P, O) -->
  " ;\n  ", serialize_resource(P),
  " ", serialize_resource(O),
  " .\n".
finish(sp, _, _, O) -->
  "\n  , ", serialize_resource(O),
  " .\n".

indent(N) --> "\n", indent_(N).
indent_(N) -->
  ( { N == 0 } -> []
  ; { 0 < N, N1 is N - 1 }, "  ", indent_(N1)
  ).

serialize_resource(iri(Iri)) --> "<", iri(Iri), ">".
serialize_resource(literal(Type, Repr)) --> serialize_repr(Repr, Type).
serialize_resource(list(Rs)) --> serialize_list(Rs).
serialize_resource(:(N)) --> ":", name(N).
serialize_resource(P:N) --> name(P), ":", name(N).

serialize_repr(@(Str, Lang), _) --> str(Str), "@", lang(Lang).
serialize_repr([], Type) --> "\"\"^^", serialize_resource(Type).
serialize_repr([H | T], Type) --> str([H | T]), "^^", serialize_resource(Type).

serialize_list([]) --> "()".
serialize_list([R | Rs]) --> "( ", serialize_list_(Rs, R), " )".

serialize_list_([], R0) --> serialize_resource(R0).
serialize_list_([R | Rs], R0) --> serialize_resource(R0), ", ", serialize_list_(Rs, R).

iri(Iri) --> foldl(iri_, Iri).
iri_(C, S0, S) :-
  if_(memberd_t(C, "\x00\\x01\\x02\\x03\\x04\\x05\\x06\\x07\\x08\\x09\\x0a\\x0b\\x0c\\x0d\\x0e\\x0f\\x10\\x11\\x12\\x13\\x14\\x15\\x16\\x17\\x18\\x19\\x1a\\x1b\\x1c\\x1d\\x1e\\x1f\ <>\"{}|^`\\"),
    ( char_code(C, N), escape_ascii_u(N, S0, S) ),
    S0 = [C | S]
  ).

name(A) --> { atom_chars(A, [C | Cs]) }, name_(C), name_after(Cs).
name_(C, S0, S) :-
  if_(memberd_t(C, "~.-!$&'()*+,;=/?#@%"),
    S0 = [(\), C | S],
    S0 = [C | S]
  ).

name_after_(C, S0, S) :-
  if_(memberd_t(C, "~-!$&'()*+,;=/?#@%"),
    S0 = [(\), C | S],
    S0 = [C | S]
  ).

name_after([]) --> [].
name_after([C | Cs]) --> name_after_(Cs, C).

name_after_([], C) --> name_(C).
name_after_([C | Cs], C0) --> name_after_(C0), name_after_(Cs, C).

str(Str) --> "\"", foldl(str_, Str), "\"".
str_(C, S0, S) :-
  if_(memberd_t(C, "\"\\"),
    S0 = [(\), C | S],
    if_(memberd_t(C, "\r\n"),
      ( if_(C = '\n', L = n, L = r), S0 = [(\), L | S] ),
      S0 = [C | S]
    )
  ).

lang(Lang) --> seq(Lang).

ascii_nums(N, N1, N0) :-
  N < 0x100, N1 is mod(div(N, 0x10), 0x10), N0 is mod(N, 0x10).

escape_percent(N) -->
  { ascii_nums(N, N1, N0) },
  "%", hex(N1), hex(N0).

escape_ascii_u(N) -->
  { ascii_nums(N, N1, N0) },
  "\\u00", hex(N1), hex(N0).

hex(0) --> "0".
hex(1) --> "1".
hex(2) --> "2".
hex(3) --> "3".
hex(4) --> "4".
hex(5) --> "5".
hex(6) --> "6".
hex(7) --> "7".
hex(8) --> "8".
hex(9) --> "9".
hex(10) --> "A".
hex(11) --> "B".
hex(12) --> "C".
hex(13) --> "D".
hex(14) --> "E".
hex(15) --> "F".
