:- module(publications_database, [
  publication_header/1, publication_body/1,
  publication_type/1,
  publication_predicates/3
]).

publication_header(["Type", "Date (yyyy-mm)", "Title", "Where", "Main Repository", "Slides"]).
publication_body(publication(Type, Date, Title, Where, MainRepository, Slides)) :-
  publication(Type, Date, Title, Where, MainRepository, Slides).

publication_type([
  text,
  date,
  link,
  link,
  link,
  link
]).

publication_predicates(3, text([
  lowercase, pospend(".pdf"), prepend("publications/"), local
]), [
  :(type),
  :(date),
  [link(text, [rdfs:label, foaf:name]), link(ref, :(doc))],
  [link(text, :(where_name)), link(ref, :(where_link))],
  link(ref, [foaf:homepage, foaf:page]),
  [link(text, :(slides_name)), link(ref, :(slides_link))]
]).

publication(
  literal("BSc Thesis"),
  year_month(2024, 02),
  name_link(
    "From Combinators to Concatenative and Back Again",
    publications("From_Combinators_to_Concatenative_and_Back_Again.pdf")
  ),
  name_link("UFRJ", https("ufrj.br/en/")),
  name_link("Pantheon", http("hdl.handle.net/11422/22871")),
  name_link("slides pt-BR",
    publications("From_Combinators_to_Concatenative_and_Back_Again_slides.pdf")
  )
).

publication(
  literal("Paper"),
  year_month(2024, 09),
  name_link(
    "Converting Combinators to and from Concatenative",
    publications("Converting_Combinators_to_and_from_Concatenative.pdf")
  ),
  name_link("SBLP2024", https("cbsoft.sbc.org.br/2024/sblp/?lang=en")),
  doi("10.5753/sblp.2024.3460"),
  name_link(
    "slides",
    publications("Converting_Combinators_to_and_from_Concatenative_slides.pdf")
  )
).
