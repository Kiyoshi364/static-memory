:- module(talks, 
[ talk_type_data/2
, talk_header/1
, talk_predicates/3
]).
talk_type_data(Type, Data) :-
  type(Type),
  G = talk(_Type, _Date, _Title, _Place, _Location, _Slides, _Extras),
  findall(G, G, Data).

type(
[ field(kind, text)
, field(date, date)
, field(title, text)
, field(place, link)
, field(location, text)
, field(slides, link)
, field(extras, list(
  or(
    [ case(internal_ref, link)
    , case(external_ref, link)
    ]),
  ", ", "", ""
  ))
]).

talk_header(["Kind", "Date (yyyy-mm[-dd])", "Title", "Place", "Location", "Slides", "Extras"]).

talk_predicates(3,
[ lowercase, pospend(".pdf"), prepend("talks/"), local
],
[ []
, []
, []
, [link(text, [rdfs:label, foaf:name])]
, []
, []
, list_each(or(
  [ internal_ref-[]
  , external_ref-[]
  ]))
]).

talk(
  literal("Meetup"),
  year_month_day(2025, 11, 14),
  literal("Towards an Implementation-Independent Interface for Reasoning about Semantic Web in Prolog"),
  text_link(
    "3rd Scryer Prolog Meetup",
    https("hsd-pbsa.de/veranstaltung/scryer-prolog-meetup-2025/")
  ),
  literal("Hochschule Düsseldorf, Düsseldorf, Germany"),
  text_link("PDF", talks("Towards_an_Implementation-Independent_Interface_for_Reasoning_about_Semantic_Web_in_Prolog.pdf")),
  [ or(internal_ref, text_link("image generators", talks("Towards_an_Implementation-Independent_Interface_for_Reasoning_about_Semantic_Web_in_Prolog/")))
  , or(external_ref, text_link("repo", mygitlab("semweb")))
  , or(external_ref, text_link("meetup discussion", https("github.com/mthom/scryer-prolog/discussions/2948")))
  ]
).

talk(
  literal("Invited"),
  year_month_day(2025, 11, 27),
  literal("Connections Between Applicative and Concatenative Tacit Programming"),
  text_link(
    "TallCat Seminars",
    https("compose.ioc.ee")
  ),
  literal("TalTech, Tallinn, Estonia"),
  text_link("PDF", talks("Connections_Between_Applicative_and_Concatenative_Tacit_Programming.pdf")),
  []
).
