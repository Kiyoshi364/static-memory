:- module(publications, 
[ publication_type_data/2
, publication_header/1
, publication_predicates/3
]).

publication_type_data(Type, Data) :-
  type(Type),
  G = publication(_Type, _Date, _Title, _Where, _MainRepository, _Extras),
  findall(G, G, Data).

type(
[ field(type, text)
, field(date, date)
, field(publication, link)
, field(where, link)
, field(main_repository, link)
, field(extras, list(
  or(
    [ case(slides, link)
    ]),
  ", ", "", ""
  ))
]).

publication_header(["Type", "Date (yyyy-mm)", "Title", "Where", "Main Repository", "Extras"]).

publication_predicates(3, text(
[ lowercase, pospend(".pdf"), prepend("publications/"), local
]),
[ []
, []
, [link(text, [rdfs:label, foaf:name])]
, []
, link(ref, [foaf:homepage, foaf:page])
, list_each(or([
    slides-[link(text, :(slides_name)), link(ref, :(slides_link))]
  ]))
]).

publication(
  literal("BSc Thesis"),
  year_month(2024, 02),
  text_link(
    "From Combinators to Concatenative and Back Again",
    publications("From_Combinators_to_Concatenative_and_Back_Again.pdf")
  ),
  text_link("UFRJ", https("ufrj.br/en/")),
  text_link("Pantheon", http("hdl.handle.net/11422/22871")),
  [ or(slides, text_link("slides pt-BR",
      publications("From_Combinators_to_Concatenative_and_Back_Again_slides.pdf")
    ))
  ]
).

publication(
  literal("Paper"),
  year_month(2024, 09),
  text_link(
    "Converting Combinators to and from Concatenative",
    publications("Converting_Combinators_to_and_from_Concatenative.pdf")
  ),
  text_link("SBLP2024", https("cbsoft.sbc.org.br/2024/sblp/?lang=en")),
  doi("10.5753/sblp.2024.3460"),
  [ or(slides, text_link(
      "slides",
      publications("Converting_Combinators_to_and_from_Concatenative_slides.pdf")
    ))
  ]
).
