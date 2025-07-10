:- module(serialize_ttl, [
  triples_predicates//4,
  serialize_prefixes//0, serialize_prefixes//2,
  serialize_triples//1
]).

:- use_module(library(lists), [
  member/2, append/3, foldl/4, maplist/3
]).
:- use_module(library(dcgs), [phrase/3, seq//1]).
:- use_module(library(reif), [if_/3, (=)/3]).

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
type_val_resources(link, Val) --> !,
  ( { Val = name_link(_, L) } -> linktarget_resource(L)
  ; { Val = doi(_)          } -> linktarget_resource(Val)
  ; { Val = mygithub(_)     } -> linktarget_resource(Val)
  ; { Val = mygitlab(_)     } -> linktarget_resource(Val)
  ; { throw(unknown_link_while_serializing(Val)) }
  ).
type_val_resources(proglang, proglang(PL)) --> !, { proglang_val(PL, Val) }, type_val_resources(link, Val).
type_val_resources(listeach(T, _, _, _), L) --> !, foldl(type_val_resources(T), L).
type_val_resources(or(Ts), O) --> !, or_resource(Ts, O).
type_val_resources(T, Val) -->
  { throw(unknown_type_val_while_converting_to_resources(T, Val)) }.

format_year_month(Y, M, S) :-
  Body = ( serialize_number(Y), "-", serialize_month(M) ),
  phrase(Body, S, []).

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

serialize_prefix(P-L) --> "@prefix ", serialize_atom(P), ": <", seq(L), "> .\n".
serialize_base_prefix(L) --> "@prefix : <", seq(L), "> .\n".

serialize_triples(Ts) --> foldl(serialize_triple, Ts).

serialize_triple(t(S, P, O)) -->
  serialize_resource(S), " ",
  serialize_resource(P), " ",
  serialize_resource(O), ".\n".

serialize_resource(iri(Iri)) --> "<", seq(Iri), ">".
serialize_resource(literal(Type, Repr)) --> serialize_repr(Repr, Type).
serialize_resource(list(Rs)) --> serialize_list(Rs).
serialize_resource(:(N)) --> ":", serialize_atom(N).
serialize_resource(P:N) --> serialize_atom(P), ":", serialize_atom(N).

serialize_atom(A) --> { atom_chars(A, As) }, seq(As).

serialize_repr(@(Str, Lang), _) --> "\"", seq(Str), "\"@", seq(Lang).
serialize_repr([], Type) --> "\"\"^^", serialize_resource(Type).
serialize_repr([H | T], Type) --> "\"", seq([H | T]), "\"^^", serialize_resource(Type).

serialize_list([]) --> "()".
serialize_list([R | Rs]) --> "( ", serialize_list_(Rs, R), " )".

serialize_list_([], R0) --> serialize_resource(R0).
serialize_list_([R | Rs], R0) --> serialize_resource(R0), ", ", serialize_list_(Rs, R).
