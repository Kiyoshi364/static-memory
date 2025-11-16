:- module(ttl,
[ serialize_prefixes//2,
  serialize_triples//1
]).

:- use_module(library(dcgs), [seq//1]).
:- use_module(library(reif), [if_/3, (=)/3, memberd_t/3]).

:- use_module(serialize, [serialize_number//1, serialize_month//1, ntfoldl//2]).

serialize_prefixes(B, Ps) --> ntfoldl(serialize_prefix, Ps), serialize_base_prefix(B).

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
  " ;\n.\n".
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

iri(Iri) --> ntfoldl(iri__, Iri).
iri__(C) --> call(iri_(C)).
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
    if_(memberd_t(C, " "),
      ( char_code(C, N), escape_percent(N, S0, S) ),
      S0 = [C | S]
    )
  ).

name_after([]) --> [].
name_after([C | Cs]) --> name_after_(Cs, C).

name_after_([], C) --> name_(C).
name_after_([C | Cs], C0) --> name_after_(C0), name_after_(Cs, C).

str(Str) --> "\"", ntfoldl(str__, Str), "\"".
str__(C) --> call(str_(C)).
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
