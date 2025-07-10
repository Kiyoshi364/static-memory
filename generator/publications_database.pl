:- module(publications_database, [
  publication_header/1, publication_predicates/2,
  publication_body/1, publication_type/1
]).

publication_header(["Type", "Date (yyyy-mm)", "Title", "Where", "Main Repository", "Slides"]).
publication_predicates(5, [[:(type)], [:(date)], [rdfs:label, foaf:name], [:(where)], [foaf:page], [:(slides)]]).
publication_body(publication(Type, Date, Title, Where, MainRepository, Slides)) :-
  publication(Type, Date, Title, Where, MainRepository, Slides).

publication_type([
  text,
  date,
  link(text),
  link(ref),
  link(ref),
  link(ref)
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
