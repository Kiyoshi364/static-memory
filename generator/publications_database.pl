:- module(publications_database, [
  publication_header/1, publication_body/1, check_publication/2
]).

:- use_module(type, [type/3]).

publication_header(['Type', 'Date (yyyy-mm)', 'Title', 'Where', 'Main Repository', 'Slides']).
publication_body(publication(Type, Date, Title,  Where, MainRepository, Slides)) :-
  publication(Type, Date, Title,  Where, MainRepository, Slides).

check_publication(P, L) :-
  functor(P, publication, L),
  type(text, P, 1),
  type(date, P, 2),
  type(link, P, 3),
  type(link, P, 4),
  type(link, P, 5),
  type(link, P, 6),
true.

publication(
  literal('BSc Thesis'),
  year_month(2024, 02),
  name_link(
    'From Combinators to Concatenative and Back Again',
    publications('From_Combinators_to_Concatenative_and_Back_Again.pdf')
  ),
  name_link('UFRJ', https('ufrj.br/en/')),
  name_link('Pantheon', http('hdl.handle.net/11422/22871')),
  name_link('slides pt-BR',
    publications('From_Combinators_to_Concatenative_and_Back_Again_slides.pdf')
  )
).

publication(
  literal('Paper'),
  year_month(2024, 09),
  name_link(
    'Converting Combinators to and from Concatenative',
    publications('Converting_Combinators_to_and_from_Concatenative.pdf')
  ),
  name_link('SBLP2024', https('cbsoft.sbc.org.br/2024/sblp/?lang=en')),
  doi('10.5753/sblp.2024.3460'),
  name_link(
    'slides',
    publications('Converting_Combinators_to_and_from_Concatenative_slides.pdf')
  )
).
