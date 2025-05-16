:- use_module(library(dcgs), [phrase/2]).
:- use_module(library(lists), [length/2, maplist/2]).
:- use_module(library(iso_ext), [setup_call_cleanup/3]).

:- use_module(serialize, [serialize_header/2, serialize_body/3]).

%%%%%%%%%%%%%%%%%%%% Publications %%%%%%%%%%%%%%%%%%%%

:- use_module(publications_database, [
  publication_header/1, publication_body/1, publication_type/1, check_publication/2
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
  check_database(publication_header, publication_body, check_publication).

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

check_database(Header_1, Body_1, Check_2) :-
  ( call(Header_1, H),
    length(H, L),
    call(Body_1, P),
    ( call(Check_2, P, L) -> true ; throw(expected_success(check(Check_2, P, L))) ),
    false
  ; true
  ).

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
