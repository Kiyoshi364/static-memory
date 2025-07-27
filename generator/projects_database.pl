:- module(projects_database, [
  project_header/1, project_body/1,
  project_type/1,
  project_predicates/3
]).

project_header(["Name", "Kind", "Summary", "Language", "Main Repository", "Mirrors", "Last Updated"]).
project_body(project(Name, Kind, Summary, Language, MainRepository, Mirrors, LastUpdated)) :-
  project(Name, Kind, Summary, Language, MainRepository, Mirrors, LastUpdated).

project_type([
  or([text, link]),
  text,
  text,
  proglang,
  link,
  list(link, ", ", "", "-"),
  date
]).

project_predicates(1, or([
  text-[lowercase, prepend("projects/"), local],
  link-text([lowercase, prepend("projects/"), local])
]), [
  or([text-[rdfs:label, foaf:name], link-link(text, [rdfs:label, foaf:name])]),
  :(kind),
  :(summary),
  [link(text, :(programming_language_name)), link(ref, :(programming_language_link))],
  link(ref, [foaf:homePage, foaf:page]),
  list_each(link(ref, foaf:page)),
  :(last_updated)
]).

project(
  or(text, literal("wam")),
  literal("library/executable"),
  literal("8-bit WAM interpreter, focusing on learning the abstract machine"),
  proglang(c),
  mygithub("wam"),
  [],
  year_month(2025, 03)
).

project(
  or(link, name_link("cbor.pl", mygitlab("cbor-pl/blob/main/cbor.pl"))),
  literal("file library"),
  literal("A prolog library for reasoning about [CBOR](https://en.wikipedia.org/wiki/CBOR)"),
  proglang(prolog),
  mygitlab("cbor-pl"),
  [mygithub("cbor-pl")],
  year_month(2025, 02)
).

project(
  or(link, name_link("struct.pl", mygitlab("struct-pl/blob/main/struct.pl"))),
  literal("file library"),
  literal("A prolog library for defining and using structs-like functors"),
  proglang(prolog),
  mygitlab("struct-pl"),
  [mygithub("struct-pl")],
  year_month(2025, 02)
).

project(
  or(text, literal("Yellowstone")),
  literal("executable"),
  literal("A Minecraft\'s Redstone inspired simulation"),
  proglang(zig),
  mygithub("yellowstone"),
  [],
  year_month(2024, 06)
).

project(
  or(text, literal("Cutils")),
  literal("many executables"),
  literal("Some self-contained small utilities written in C"),
  proglang(c),
  mygithub("cutils"),
  [],
  year_month(2024, 06)
).

project(
  or(text, literal("crlf")),
  literal("executable"),
  literal("Converts files from linux to windows text file format or the other way around"),
  proglang(zig),
  mygithub("crlf"),
  [],
  year_month(2022, 08)
).
