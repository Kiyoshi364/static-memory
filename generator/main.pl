:- use_module(library(dcgs), [phrase/3, phrase//1, seq//1]).
:- use_module(library(lists), [append/3, length/2, maplist/2]).
:- use_module(library(pio), [phrase_to_stream/2]).
:- use_module(library(iso_ext), [setup_call_cleanup/3]).

:- use_module(me,
[ rdf_me/1, rdf_prefixes/2, me_triples//0
, mygithub/1
]).
:- use_module(type, [check_field/3]).

:- use_module(serialize/serialize, [maplistfunc/3, ntfoldl//2]).

:- use_module(serialize/md, [serialize_md_header//1, serialize_md_body//2]).

:- use_module(serialize/typst, [serialize_typst_named_listof_dictionary//3]).

:- use_module(serialize/triples, [triples_predicates//6, check_triplification/4]).
:- use_module(serialize/ttl, [serialize_prefixes//2, serialize_triples//1]).

serializations(
[ ser(typst, "typ", text(typst, "Typst"))
, ser(triples_ttl, "ttl", text(triples, "Turtle RDF Triples"))
]).

% db(Name, Type_Data_2, Header_1, Predicates_3)
databases(
[ db(publication, publication_type_data, publication_header, publication_predicates)
, db(talk, talk_type_data, talk_header, talk_predicates)
, db(project, project_type_data, project_header, project_predicates)
]).

:- use_module(database/publications, [publication_type_data/2, publication_header/1, publication_predicates/3]).
:- use_module(database/talks, [talk_type_data/2, talk_header/1, talk_predicates/3]).
:- use_module(database/projects, [project_type_data/2, project_header/1, project_predicates/3]).

%%%%%%%%%%%%%%%%%%%% Markdown %%%%%%%%%%%%%%%%%%%%

markdown(SerDir, FilePrefix) -->
  md_preamble,
  md_databases,
  md_about(SerDir),
  md_serializations(SerDir, FilePrefix),
[].

md_preamble -->
  { mygithub(GITHUB) },
  "# Static Memory\n\n",
  "This markdown is avaliable at [", seq(GITHUB), "/static-memory](", seq(GITHUB), "/static-memory).\n",
[].

md_databases -->
  { databases(DBs) },
  ntfoldl(md_database_db, DBs).

md_database_db(db(Name, Type_Data_2, Header_1, _Predicates_3)) -->
  md_preamble(Name),
  md_database(Header_1, Type_Data_2).

md_preamble(publication) --> !,
  "\n## Publications\n\n",
  "The link from _Title_ is local to the git repository.\n",
  "The link from _Main Repository_ is to somewhere else,\n",
  "you probably should use the link in this column to refer/cite/share.\n",
  "\n",
[].
md_preamble(talk) --> !,
  "\n## Talks\n\n",
[].
md_preamble(project) --> !,
  "\n## Programming Projects\n\n",
[].
md_preamble(Name) -->
  { throw(error(unknown_md_preamble(Name))) }.

md_database(Header_1, Type_Data_2) -->
  { call(Header_1, H) },
  serialize_md_header(H),
  { call(Type_Data_2, T, Bs) },
  ntfoldl(serialize_md_body(T), Bs).

md_about(SerDir) -->
  "\n## About ...\n",
  "\n### About me\n\n",
  "I am Daniel K Hashimoto (my first name is Daniel Kiyoshi),\n",
  "a master student at [PPGI (UFRJ)](https://ppgi.ufrj.br/).\n",
  "My github account is [Kiyoshi364](https://github.com/Kiyoshi364),\n",
  "my gitlab is [Hashi364](https://gitlab.com/Hashi364).\n",
  "\n",
  "If you run `$ sha1sum \"mailto:$EMAIL\"` (where `$EMAIL` is my email),\n",
  "you should get `a638c7eafa7ba4bbe8b9cab7281113798d09da13`.\n",
  "\n### About this markdown\n\n",
  "I called this markdown \"static-memory\"\n",
  "because [static memory](https://en.wikipedia.org/wiki/Static_variable)\n",
  "refers to a memory segment\n",
  "which holds global constants and variables\n",
  "allocated at the start of the program\n",
  "(in opposition to the ones allocated on the heap or the stack).\n",
  "And so does this markdown.\n",
  "\n",
  "This markdown format was inspired by [github.com/codereport/Content](https://github.com/codereport/Content).\n",
  "It was generated via a Prolog script (see next subsubsection).\n",
  "\n### About the generator\n\n",
  "From machine-readable *data files*,\n",
  "a Prolog script generates the tables in this readme\n",
  "and some other data files in different formats in `", seq(SerDir), "`.\n",
  "\n",
  "That's neat, right?\n",
  "If you think so too,\n",
  "feel free to take a look and inspire yourself",
  "from the generator's code.\n",
  "However, I won't give you any licences for it.\n",
  "Instead I encourage you to build your own generator\n",
  "specially tailored for your own needs.\n",
  "\n",
  "If you choose to make your own generator,\n",
  "I suggest starting small and adding more as you go.\n",
  "I also suggest spliting the code in some parts:\n",
  "  * database: a human-writable and machine-readable file\n",
  "    to hold your data.\n",
  "    I use the script language for that,\n",
  "    but you can also use any data format\n",
  "    (JSON, TOML, YAML, ZIGGY, ...).\n",
  "  * serializer: a code that transforms your data\n",
  "    into your desired format,\n",
  "    such as a markdown table, a PDF, other data formats, ...\n",
  "  * (optional) checking: a code to verify that\n",
  "    you have not done any silly mistakes\n",
  "    while hand-editing the database file(s).\n",
  "    You do not have to do this,\n",
  "    but I am a bit paranoid,\n",
  "    so I do it anyways.\n",
  "  * script: a glue code to\n",
  "    read the database(, check it) and serialize it.\n",
  "\n",
  "Consider taking a look at the commit\n",
  "`398266b20b5e119216b4116ee78b78e1757c53bb`.\n",
[].

md_serialization_preamble(SerDir) -->
  "\n## Available Serializations\n\n",
  "Machine-readable data\n",
  "about what is in this `readme.md`\n",
  "is available in [", seq(SerDir), "](./", seq(SerDir), ") folder.\n",
[].

md_serializations(SerDir, FilePrefix) -->
  { serializations(Sers) },
  md_serialization_preamble(SerDir),
  ntfoldl(md_serialization_ser(SerDir, FilePrefix), Sers).

md_serialization_ser(SerDir, FilePrefix, ser(Name, FileExt, Encoding)) -->
  md_serialization(SerDir, FilePrefix, Name, FileExt, Encoding).

md_serialization(SerDir, FilePrefix, Name, FileExt, Encoding) -->
  { file_prefix_ext_name(FilePrefix, FileExt, FileName) },
  md_serialization_specific_preamble(Name, SerDir, FileName),
  md_serialization_encoding(Encoding, FileExt),
[].

md_serialization_encoding(text(NT__0, Details), FileExt) -->
  open_details(Details),
  "```", seq(FileExt), "\n",
  phrase(NT__0),
  "```\n",
  close_details,
[].

md_serialization_specific_preamble(typst, SerDir, FileName) --> !,
  "\n### [Typst](https://typst.app)\n\n",
  also_available_at(SerDir, FileName),
  "\n".
md_serialization_specific_preamble(triples_ttl, SerDir, FileName) --> !,
  triples_preamble(SerDir, FileName).
md_serialization_specific_preamble(Name, SerDir, FileName) -->
  { throw(error(unknown_md_serialization_specific_preamble(Name, SerDir, FileName))) }.

%%%%%%%%%%%%%%%%%%%% TYPST %%%%%%%%%%%%%%%%%%%%

typst -->
  { databases(DBs) },
  ntfoldl(typst_database_db, DBs).

typst_database_db(db(Name, Type_Data_2, _Header_1, _Predicates_3)) -->
  typst_database(Name, Type_Data_2).

typst_database(Name, Type_Data_2) -->
  { call(Type_Data_2, T, Bs) },
  serialize_typst_named_listof_dictionary(Name, T, Bs).

%%%%%%%%%%%%%%%%%%%% RDF %%%%%%%%%%%%%%%%%%%%

triples -->
  { rdf_prefixes(B, Ps),
    Body =
    ( me_triples
    , triples_databases
    ),
    phrase(Body, Ts, [])
  },
  serialize_prefixes(B, Ps),
  "\n",
  serialize_triples(Ts).

triples_databases -->
  { databases(DBs) },
  ntfoldl(triples_database_db, DBs).

triples_database_db(db(_Name, Type_Data_2, _Header_1, Predicates_3)) -->
  triples_database(Type_Data_2, Predicates_3).

triples_database(Type_Data_2, Predicates_3) -->
  { call(Predicates_3, SubN, SubEx, Ps),
    call(Type_Data_2, T, Bs),
    rdf_me(Me)
  },
  ntfoldl(triples_predicates(T, Ps, Me, SubN, SubEx), Bs).

triples_preamble(SerDir, FileTtl) -->
  "\n### RDF Triples ([Turtle](https://en.wikipedia.org/wiki/Turtle_(syntax)))\n\n",
  also_available_at(SerDir, FileTtl),
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
  "The `:`-prefixed predicates are pretty much stable\n",
  "(but don't trust me),\n",
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

%%%%%%%%%%%%%%%%%%%% HELPERS %%%%%%%%%%%%%%%%%%%%

open_details(Summary) --> "<details><summary>", seq(Summary), "</summary>\n\n".
close_details --> "</details>\n".

also_available_at(SerDir, File) -->
  "This data is also available at [", seq(SerDir), seq(File), "](./", seq(SerDir), seq(File), ").\n",
[].

file_prefix_ext_name(FilePrefix, FileExt, FileName) :-
  append(FilePrefix, ['.' | FileExt], FileName).

cassert(Goal) :-
  ( \+ call(Goal) -> throw(expected_success(Goal))
  ; call(Goal)
  ).
cassert(Goal, A) :-
  ( \+ call(Goal, A) -> throw(expected_success(Goal, A))
  ; call(Goal, A)
  ).

check_databases :-
  databases(DBs),
  maplist(check_database_db, DBs).

check_database_db(db(Name, Type_Data_2, Header_1, Predicates_3)) :-
 check_database(Name, Type_Data_2, Header_1, Predicates_3).

check_database(Name, Type_Data_2, Header_1, Predicates_3) :-
  call(Type_Data_2, Ts, Bs),
  cassert(length(Ts, L)),
  call(Header_1, H),
  length(H, L),
  maplist(cassert(check_body(Ts, Name)), Bs),
  call(Predicates_3, SubN, SubEx, Ps),
  cassert(length(Ps, L)),
  cassert(SubN < L),
  cassert(check_triplification(Ts, SubN, SubEx, Ps)).

check_body(Ts, Name, B) :-
  functor(B, Name, _),
  maplistfunc(check_body_(B), Ts, B).

check_body_(B, T, Val) :- check_field(T, B, Val).

%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%

main :- main_md_all('readme.md', "serializations/", "static-memory").

main_md_all(FileMd, SerDir, FilePrefix) :-
  check_databases,
  setup_call_cleanup(
    open(FileMd, write, S, []),
    run_md(S, SerDir, FilePrefix),
    close(S)
  ),
  run_serializations(SerDir, FilePrefix).

run_serializations(SerDir, FilePrefix) :-
  serializations(Sers),
  maplist(run_serialization(SerDir, FilePrefix), Sers).

run_serialization(SerDir, FilePrefix, ser(_Name, FileExt, Encoding)) :-
  append(SerDir, FileName, OpenPath),
  file_prefix_ext_name(FilePrefix, FileExt, FileName),
  run_serialization_(Encoding, OpenPath),
true.

run_serialization_(text(NT__0, _), OpenPath) :-
  setup_call_cleanup(
    open(OpenPath, write, S, []),
    phrase_to_stream(NT__0, S),
    close(S)
  ).

run_md(S, SerDir, FilePrefix) :-
  phrase_to_stream(markdown(SerDir, FilePrefix), S).
