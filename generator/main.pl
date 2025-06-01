:- use_module(library(dcgs), []).
:- use_module(library(lists), [length/2]).
:- use_module(library(pio), [phrase_to_stream/2]).
:- use_module(library(iso_ext), [setup_call_cleanup/3]).

:- use_module(serialize, [serialize_header//1, serialize_body//2]).
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
  "\n",
[].

check_publications :-
  check_database(publication, publication_header, publication_body, publication_type).

write_publications(S) :-
  phrase_to_stream(publications_preamble, S),
  serialize_database(S, publication_header, publication_body, publication_type).

%%%%%%%%%%%%%%%%%%%% Projects %%%%%%%%%%%%%%%%%%%%

:- use_module(projects_database, [
  project_header/1, project_body/1, project_type/1
]).

projects_preamble -->
  "\n## Projects\n\n",
[].

check_projects :-
  check_database(project, project_header, project_body, project_type).

write_projects(S) :-
  phrase_to_stream(projects_preamble, S),
  serialize_database(S, project_header, project_body, project_type).

%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%

serialize_database(S, Header_1, Body_1, Type_1) :-
  call(Header_1, H),
  phrase_to_stream(serialize_header(H), S),
  call(Type_1, T),
  ( call(Body_1, B),
    phrase_to_stream(serialize_body(T, B), S),
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
  check_projects,
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
  write_projects(S),
true.
