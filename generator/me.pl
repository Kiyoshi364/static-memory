:- module(me, [
  rdf_me/1, rdf_prefixes/2, me_triples//0,
  mygithub/1, mygitlab/1
]).

mygithub("github.com/Kiyoshi364").
mygitlab("gitlab.com/Hashi364").

rdf_me(:(me)).

rdf_prefixes("http://github.com/Kiyoshi364/static-memory#", [
  foaf - "http://xmlns.com/foaf/0.1/",
  rdf - "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  rdfs - "http://www.w3.org/2000/01/rdf-schema#",
  xsd - "http://www.w3.org/2001/XMLSchema#"
]).

me_triples -->
  { rdf_me(Me),
    Homepage = iri("https://github.com/Kiyoshi364/static-memory"),
    SchoolHomepage = iri("https://dcc.ufrj.br/")
  },
  [ t(Me, rdf:type, foaf:'Person'),
    t(Me, rdfs:label, literal(rdf:lang_string, @("Daniel K Hashimoto", "pt-BR"))),
    t(Me, foaf:name, literal(rdf:lang_string, @("Daniel K Hashimoto", "pt-BR"))),
    t(Me, foaf:firstName, literal(rdf:lang_string, @("Daniel Kiyoshi", "pt-BR"))),
    t(Me, foaf:nick, literal(xsd:string, "Hashi364")),
    t(Me, foaf:nick, literal(xsd:string, "Kiyoshi364")),
    t(Me, foaf:homepage, Homepage),
    t(Me, foaf:publications, Homepage),
    t(Me, foaf:schoolHomepage, SchoolHomepage),
    t(Me, foaf:mbox_sha1sum, literal(xsd:hexBinary, "a638c7eafa7ba4bbe8b9cab7281113798d09da13"))
  ].
