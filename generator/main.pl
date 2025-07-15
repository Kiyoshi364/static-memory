:- use_module(library(dcgs), [phrase/3]).
:- use_module(library(lists), [length/2, foldl/4, nth1/3]).
:- use_module(library(pio), [phrase_to_stream/2]).
:- use_module(library(iso_ext), [setup_call_cleanup/3]).

:- use_module(me, [me_triples//0, mygithub/1]).
:- use_module(type, [type/3, valid_subject_type/1]).

:- use_module(serialize_md, [serialize_header//1, serialize_body//2]).
:- use_module(serialize_ttl, [triples_predicates//4, serialize_prefixes//0, serialize_triples//1]).

%%%%%%%%%%%%%%%%%%%% Publications %%%%%%%%%%%%%%%%%%%%

:- use_module(publications_database, [
  publication_header/1, publication_predicates/2, publication_body/1, publication_type/1
]).

check_publications :-
  check_database(publication, publication_header, publication_predicates, publication_body, publication_type).

publications_preamble -->
  "\n## Publications\n\n",
  "The link from _Title_ is local to the git repository.\n",
  "The link from _Main Repository_ is to somewhere else,\n",
  "you probably should use the link in this column to refer/cite/share.\n",
  "\n",
[].

publications -->
  publications_preamble,
  serialize_database(publication_header, publication_body, publication_type).

publications_triples -->
  triples_database(publication_predicates, publication_body, publication_type).

%%%%%%%%%%%%%%%%%%%% Projects %%%%%%%%%%%%%%%%%%%%

:- use_module(projects_database, [
  project_header/1, project_predicates/2, project_body/1, project_type/1
]).

check_projects :-
  check_database(project, project_header, project_predicates, project_body, project_type).

projects_preamble -->
  "\n## Programming Projects\n\n",
[].

projects -->
  projects_preamble,
  serialize_database(project_header, project_body, project_type).

projects_triples -->
  triples_database(project_predicates, project_body, project_type).

%%%%%%%%%%%%%%%%%%%% RDF %%%%%%%%%%%%%%%%%%%%

triples_database(Predicates_2, Body_1, Type_1) -->
  { call(Predicates_2, SubN, Ps),
    call(Type_1, T),
    findall(B, call(Body_1, B), Bs)
  },
  foldl(triples_predicates(T, Ps, SubN), Bs).

triples_preamble(FileTtl) -->
  "\n## Triples\n\n",
  "Machine-readable data (in [Turtle](https://en.wikipedia.org/wiki/Turtle_(syntax)))\n",
  "about what is in this `readme.md`.\n",
  "This data is also available at [", atom(FileTtl), "](./", atom(FileTtl), ").\n",
  "\n",
[].

triples_md(FileTtl) -->
  triples_preamble(FileTtl),
  "```ttl\n",
  triples,
  "```\n",
[].

triples -->
  { Body = (
      me_triples,
      publications_triples,
      projects_triples
    ),
    phrase(Body, Ts, [])
  },
  serialize_prefixes,
  "\n",
  serialize_triples(Ts).

atom(A) --> { atom_chars(A, As) }, As.

%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%

serialize_database(Header_1, Body_1, Type_1) -->
  { call(Header_1, H) },
  serialize_header(H),
  { call(Type_1, T),
    findall(B, call(Body_1, B), Bs)
  },
  foldl(serialize_body(T), Bs),
  "\n",
[].

cassert(Goal) :-
  ( \+ call(Goal) -> throw(expected_success(Goal))
  ; call(Goal)
  ).

check_database(Name, Header_1, Predicates_2, Body_1, Type_1) :-
  call(Header_1, H),
  length(H, L),
  call(Predicates_2, SubjN, Ps),
  cassert(length(Ps, L)),
  cassert(SubjN < L),
  call(Type_1, Ts),
  cassert(length(Ts, L)),
  cassert(( nth1(SubjN, Ts, TSubj), valid_subject_type(TSubj) )),
  ( call(Body_1, B),
    cassert(check_body(Ts, Name, B)),
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

main_md :- check_databases, current_output(S), run_md(S, 'ttl.ttl').
main_ttl :- check_databases, current_output(S), run_ttl(S).

main_md_ttl(FileMd, FileTtl) :-
  check_databases,
  setup_call_cleanup(
    open(FileMd, write, MD, []),
    run_md(MD, FileTtl),
    close(MD)
  ),
  setup_call_cleanup(
    open(FileTtl, write, TTL, []),
    run_ttl(TTL),
    close(TTL)
  ).

md_preamble -->
  { mygithub(GITHUB) },
  "# Static Memory\n",
  "\n",
  "This markdown is avaliable at [", GITHUB, "/static-memory](", GITHUB, "/static-memory).\n",
[].

run_md(S, FileTtl) :-
  Body = (
    md_preamble,
    publications,
    projects,
    triples_md(FileTtl)
  ),
  phrase_to_stream(Body, S).

run_ttl(S) :-
  phrase_to_stream(triples, S).
