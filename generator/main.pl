:- use_module(library(dcgs), [phrase/2]).
:- use_module(library(lists), [length/2, maplist/2]).
:- use_module(library(iso_ext), [setup_call_cleanup/3]).

:- use_module(serialize, [serialize_header/2, serialize_body/3]).
:- use_module(type, [type/3]).

%%%%%%%%%%%%%%%%%%%% Publications %%%%%%%%%%%%%%%%%%%%

:- use_module(publications_database, [
  publication_header/1, publication_body/1, publication_type/1
]).

publications_preamble -->
  "\n## Publications\n\n",
  "The link from _Title_ is local to the git repository.\n",
  "The link from _Main Repository_ is to somewhere else,\n",
  "you probably should use the link in this column to refer/cite/share.\n",
[].

write_publications(S) :-
  phrase(publications_preamble, Preamble),
  serialize_database(S, Preamble, publication_header, publication_type, publication_body),
true.

check_publications :-
  check_database(publication, publication_header, publication_body, publication_type).

%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%

serialize_database(S, Preamble, Header_1, Type_1, Body_1) :-
  maplist(write(S), Preamble),
  nl(S),
  call(Header_1, H),
  serialize_header(S, H),
  call(Type_1, T),
  ( call(Body_1, B),
    serialize_body(S, T, B),
    false
  ; true
  ),
  nl(S),
true.

check_database(Name, Header_1, Body_1, Type_1) :-
  ( call(Header_1, H),
    length(H, L),
    call(Type_1, Ts),
    length(Ts, L),
    call(Body_1, B),
    ( check_body(Ts, Name, B) -> true ; throw(expected_success(check_body(Ts, Name, B))) ),
    false
  ; true
  ).

check_body(Ts, Name, B) :-
  functor(B, Name, Arity),
  check_body_(Ts, B, 1, Arity).

check_body_([], _, N, Arity) :- N is Arity + 1.
check_body_([T | Ts], B, N, Arity) :-
  N =< Arity,
  N1 is N + 1,
  type(T, B, N),
  check_body_(Ts, B, N1, Arity).

check_databases :-
  check_publications,
true.

main :- check_databases, current_output(S), run(S).
main_file(F) :-
  check_databases,
  setup_call_cleanup(
    open(F, write, S, []),
    run(S),
    close(S)
  ).

run(S) :-
  write(S, '# Static Memory\n'),
  write_publications(S),
true.
