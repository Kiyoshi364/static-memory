:- module(projects,
[ project_type_data/2,
  project_header/1,
  project_predicates/3
]).

project_type_data(Type, Data) :-
  type(Type),
  G = project(_Name, _Kind, _Summary, _Language, _Repositories, _LastUpdated),
  findall(G, G, Data).

type(
[ field(name, or([case(text, text), case(link, link)]))
, field(kind, text)
, field(summary, text)
, field(programming_language, proglang)
, field(repositories, list(
  or(
    [ case(main, link)
    , case(mirror, link)
    ]),
  ", ", "", "-"))
, field(last_updated, date)
]).

project_header(["Name", "Kind", "Summary", "Language", "Repositories", "Last Updated"]).

project_predicates(1, or(
[ text-[lowercase, prepend("projects/"), local]
, link-text([lowercase, prepend("projects/"), local])
]),
[ or([text-[rdfs:label, foaf:name], link-link(text, [rdfs:label, foaf:name])])
, []
, []
, []
, list_each(or(
  [ main-[link(ref, foaf:page), link(ref, foaf:homePage)]
  , mirror-[link(ref, foaf:page)]
  ]))
, []
]).

project(
  or(text, literal("wam")),
  literal("library/executable"),
  literal("8-bit WAM interpreter, focusing on learning the abstract machine"),
  proglang(c),
  [ or(main, mygithub("wam"))
  ],
  year_month(2025, 03)
).

project(
  or(link, text_link("cbor.pl", mygitlab("cbor-pl/blob/main/cbor.pl"))),
  literal("file library"),
  literal("A prolog library for reasoning about [CBOR](https://en.wikipedia.org/wiki/CBOR)"),
  proglang(prolog),
  [ or(main, mygithub("cbor-pl"))
  ],
  year_month(2025, 02)
).

project(
  or(link, text_link("struct.pl", mygitlab("struct-pl/blob/main/struct.pl"))),
  literal("file library"),
  literal("A prolog library for defining and using structs-like functors"),
  proglang(prolog),
  [ or(main, mygitlab("struct-pl"))
  , or(mirror, mygithub("struct-pl"))
  ],
  year_month(2025, 02)
).

project(
  or(text, literal("Yellowstone")),
  literal("executable"),
  literal("A Minecraft\'s Redstone inspired simulation"),
  proglang(zig),
  [ or(main, mygithub("yellowstone"))
  ],
  year_month(2024, 06)
).

project(
  or(text, literal("Cutils")),
  literal("many executables"),
  literal("Some self-contained small utilities written in C"),
  proglang(c),
  [ or(main, mygithub("cutils"))
  ],
  year_month(2024, 06)
).

project(
  or(text, literal("Queue Simulator")),
  literal("undergrad group assignment"),
  literal("Discrete-event simulator for [queue](https://en.wikipedia.org/wiki/Queueing_theory) statistics. I did the design and implementation of the simulator"),
  proglang(c),
  [ or(main, text_link("github.com/PolyTadeu/TrabFinalAD", https("github.com/PolyTadeu/TrabFinalAD")))
  ],
  year_month(2022, 08)
).

project(
  or(text, literal("crlf")),
  literal("executable"),
  literal("Converts files from linux to windows text file format or the other way around"),
  proglang(zig),
  [ or(main, mygithub("crlf"))
  ],
  year_month(2022, 08)
).
