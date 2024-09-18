:- use_module(library(lists), [length/2]).
:- use_module(library(iso_ext), [setup_call_cleanup/3]).

:- use_module(serialize, [serialize_header/2, serialize_body/2]).

:- use_module(publications_database, [
  publication_header/1, publication_body/1, check_publication/2
]).

serialize_publication_table(S) :-
  publication_header(H),
  serialize_header(S, H),
  ( publication_body(Pub),
    serialize_body(S, Pub),
    false
  ; true
  ).

write_publications(S) :-
  write(S, '\n## Publications\n\n'),
  write(S, 'The link from _Title_ is local to the git repository.'), nl(S),
  write(S, 'The link from _Main Repository_ is to somewhere else,'), nl(S),
  write(S, 'you probably should use the link in this column to refer/cite/share.'), nl(S),
  nl(S),
  serialize_publication_table(S),
  nl(S),
true.

check_publications :-
  ( publication_header(H),
    length(H, L),
    publication_body(P),
    ( check_publication(P, L) -> true ; throw(expected_success(check_publication(P, L))) ),
    false
  ; true
  ).

check_database :-
  check_publications,
true.

main :- check_database, current_output(S), run(S).
main_file(F) :-
  setup_call_cleanup(
    open(F, write, S, []),
    run(S),
    close(S)
  ).

run(S) :-
  check_database,
  write(S, '# Static Memory\n'),
  write_publications(S),
true.
