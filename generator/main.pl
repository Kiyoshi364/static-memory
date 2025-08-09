:- use_module(library(dcgs), [phrase/3, seq//1]).
:- use_module(library(lists), [append/3, foldl/4, length/2]).
:- use_module(library(pio), [phrase_to_stream/2]).
:- use_module(library(iso_ext), [setup_call_cleanup/3]).

:- use_module(me, [
  rdf_me/1, rdf_prefixes/2, me_triples//0,
  mygithub/1
]).
:- use_module(type, [type/3]).

:- use_module(serialize/md, [serialize_header//1, serialize_body//2]).

:- use_module(serialize/triples, [triples_predicates//6, check_triplification/4]).
:- use_module(serialize/ttl, [serialize_prefixes//2, serialize_triples//1]).

%%%%%%%%%%%%%%%%%%%% Publications %%%%%%%%%%%%%%%%%%%%

:- use_module(database/publications, [
  publication_header/1, publication_body/1, publication_type/1, publication_predicates/3
]).

check_publications :-
  check_database(publication, publication_header, publication_body, publication_type, publication_predicates).

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
  triples_database(publication_body, publication_type, publication_predicates).

%%%%%%%%%%%%%%%%%%%% Projects %%%%%%%%%%%%%%%%%%%%

:- use_module(database/projects, [
  project_header/1, project_body/1, project_type/1, project_predicates/3
]).

check_projects :-
  check_database(project, project_header, project_body, project_type, project_predicates).

projects_preamble -->
  "\n## Programming Projects\n\n",
[].

projects -->
  projects_preamble,
  serialize_database(project_header, project_body, project_type).

projects_triples -->
  triples_database(project_body, project_type, project_predicates).

%%%%%%%%%%%%%%%%%%%% Markdown %%%%%%%%%%%%%%%%%%%%

markdown(Dir, FileTtl) -->
  md_preamble,
  publications,
  projects,
  md_about,
  md_serialization(Dir, FileTtl),
[].

md_preamble -->
  { mygithub(GITHUB) },
  "# Static Memory\n\n",
  "This markdown is avaliable at [", seq(GITHUB), "/static-memory](", seq(GITHUB), "/static-memory).\n",
[].

md_about -->
  "\n## About\n\n",
  "This markdown format was inspired by [github.com/codereport/Content](https://github.com/codereport/Content)\n",
[].

md_serialization_preamble(Dir) -->
  "\n## Available Serializations\n\n",
  "Machine-readable data\n",
  "about what is in this `readme.md`\n",
  "is available in [", seq(Dir), "](./", seq(Dir), ") folder.\n",
[].

md_serialization(Dir, FileTtl) -->
  md_serialization_preamble(Dir),
  triples_md(Dir, FileTtl).

%%%%%%%%%%%%%%%%%%%% RDF %%%%%%%%%%%%%%%%%%%%

triples_database(Body_1, Type_1, Predicates_3) -->
  { call(Predicates_3, SubN, SubEx, Ps),
    call(Type_1, T),
    rdf_me(Me),
    findall(B, call(Body_1, B), Bs)
  },
  foldl(triples_predicates(T, Ps, Me, SubN, SubEx), Bs).

triples_preamble(Dir, FileTtl) -->
  "\n### RDF Triples ([Turtle](https://en.wikipedia.org/wiki/Turtle_(syntax)))\n\n",
  "This data is also available at [", seq(Dir), seq(FileTtl), "](./", seq(Dir), seq(FileTtl), ").\n",
  "\n",
  "> [!WARNING]\n",
  "> I don't own a server,\n",
  "> so it is hard to do [Cool URIs](https://w3.org/TR/2008/NOTE-cooluris-20081203).\n",
  "> The only URIs I consider what are safe to dereference\n",
  "> are the one surrounded by `<>`.\n",
  "> In another words,\n",
  "> DO NOT dereference URIs from the `:` prefix.\n",
  "\n",
  "Everything with the `:` prefix is custom made.\n",
  "So, there is no formal specification/ontology for them.\n",
  "The `:`-prefixed predicates are pretty much stable,\n",
  "they are based on the tables from this readme.\n",
  "However, the `:`-prefixed things/objects are experimental.\n",
  "Because of that,\n",
  "consider using a structural approach\n",
  "instead of relying on the resources identifiers.\n",
  "For example,\n",
  "find a publication via\n",
  "its name (`rdfs:label`)\n",
  "or its homepage (`foaf:homepage`).\n",
  "([Cool URIs - Section 6.2. Reference by Description](https://w3.org/TR/2008/NOTE-cooluris-20081203#blanknodes)\n",
  "has some info on what I mean by this structural approach)\n",
  "\n",
[].

triples_md(Dir, FileTtl) -->
  triples_preamble(Dir, FileTtl),
  "<details><summary>Turtle RDF Triples</summary>\n",
  "\n",
  "```ttl\n",
  triples,
  "```\n",
  "</details>\n",
[].

triples -->
  { rdf_prefixes(B, Ps),
    Body = (
      me_triples,
      publications_triples,
      projects_triples
    ),
    phrase(Body, Ts, [])
  },
  serialize_prefixes(B, Ps),
  "\n",
  serialize_triples(Ts).

atom(A) --> { atom_chars(A, As) }, seq(As).

%%%%%%%%%%%%%%%%%%%% HELPERS %%%%%%%%%%%%%%%%%%%%

serialize_database(Header_1, Body_1, Type_1) -->
  { call(Header_1, H) },
  serialize_header(H),
  { call(Type_1, T),
    findall(B, call(Body_1, B), Bs)
  },
  foldl(serialize_body(T), Bs),
[].

cassert(Goal) :-
  ( \+ call(Goal) -> throw(expected_success(Goal))
  ; call(Goal)
  ).

check_database(Name, Header_1, Body_1, Type_1, Predicates_3) :-
  call(Header_1, H),
  length(H, L),
  call(Type_1, Ts),
  cassert(length(Ts, L)),
  ( call(Body_1, B),
    cassert(check_body(Ts, Name, B)),
    false
  ; true
  ),
  call(Predicates_3, SubN, SubEx, Ps),
  cassert(length(Ps, L)),
  cassert(SubN < L),
  cassert(check_triplification(Ts, SubN, SubEx, Ps)).

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

%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%

main :- main_md_ttl('readme.md', "serializations/", "static-memory.ttl").
main_md :- check_databases, current_output(S), run_md(S, "./",  "ttl.ttl").
main_ttl :- check_databases, current_output(S), run_ttl(S).

main_md_ttl(FileMd, Dir, FileTtl) :-
  check_databases,
  setup_call_cleanup(
    open(FileMd, write, MD, []),
    run_md(MD, Dir, FileTtl),
    close(MD)
  ),
  append(Dir, FileTtl, OpenPathTtl),
  setup_call_cleanup(
    open(OpenPathTtl, write, TTL, []),
    run_ttl(TTL),
    close(TTL)
  ).

run_md(S, Dir, FileTtl) :-
  phrase_to_stream(markdown(Dir, FileTtl), S).

run_ttl(S) :-
  phrase_to_stream(triples, S).
