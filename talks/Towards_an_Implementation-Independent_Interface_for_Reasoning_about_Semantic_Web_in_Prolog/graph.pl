:- module(graph, [
  gengraph/1, gengraph/2,
  prolog_graph//1
]).

:- use_module(library(format), [format_/2]).
:- use_module(library(dcgs), []).
:- use_module(library(lists), [foldl/4, member/2]).
:- use_module(library(pio), [phrase_to_file/2]).

:- use_module(dot).

gengraph(File) :- gengraph(File, 'wiki').
gengraph(File, Database) :- phrase_to_file(prolog_graph(Database), File).

prolog_graph(Database) -->
"strict digraph {\n",
  prolog_graph_(Database),
"}".

prolog_graph_(svo) -->
  tab, "rankdir=LR;\n",
  simple_triple('Subj', 'Obj', [label="   Pred   "]),
[].
prolog_graph_(open_world) -->
  tab, "rankdir=LR;\n",
  simple_triple('Prolog', 'Datalog', [label="   influenced   "]),
  simple_triple('Prolog', 'C', [label="   not(influenced)   "]),
[].
prolog_graph_(Database) -->
  { \+ member(Database, [svo, open_world]),
    consult(Database)
  },
  custom_start(Database),
  "\n",
  influence,
  first_appearence,
  "\n",
  style_langs,
  style_dates,
  "\n",
  custom_end(Database).

custom_start(wiki) -->
  tab, "rankdir=TB;\n",
  tab, "nodesep=1;\n",
  tab, "ranksep=1;\n",
[].
custom_start(blank) -->
[].

custom_end(wiki) -->
  tab, same_rank(graph, [lang('Prolog'), lang('Datalog'), lang('SQL')]),
[].
custom_end(blank) -->
  { X = a },
  tab, edge(lang(blank('Erlang')), invis(X), [style=invis]),
  tab, style_invis(X),
  tab, same_rank(graph, [lang(blank('Erlang')), invis(X)]),
[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple_triple(S, O, Attrs) -->
  tab, edge(lang(S), lang(O), Attrs),
  style_lang(S),
  style_lang(O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

influence -->
  { findall(P-C, user:influenced(P, C), PCs) },
  foldl(influenced, PCs).

influenced(P-C) --> tab, edge(lang(P), lang(C), [xlabel="  influenced  "]).

first_appearence -->
  { findall(L-D, user:first_appeared(L, D), LDs) },
  foldl(first_appeared, LDs).

first_appeared(L-D) -->
  tab, edge(lang(L), date(D), [xlabel="  first_appeared  "]).

style_langs -->
  { findall(L, ( user:influenced(A, B), (L = A ; L = B) ), Ls0), sort(Ls0, Ls) },
  foldl(style_lang, Ls).

style_lang(blank(L)) -->
  tab, lang(L), attrs([
    shape=circle, width='0.25',
    style=filled,color=black,fillcolor=gray15,
    label=""
  ]), ";\n".

style_lang(L) -->
  { atom(L) },
  tab, lang(L), attrs([shape=oval,style=filled,color=blue,fillcolor=gray90]), ";\n".

lang(blank(L)) --> format_("~a", [L]).
lang(L) --> { atom(L) }, format_("~a", [L]).

style_invis(X) --> invis(X), attrs([style=invis]), ";\n".

invis(X) --> format_("~a", [X]).

style_dates -->
  { findall(D, user:first_appeared(_, D), Ds) },
  foldl(style_date, Ds).

style_date(D) -->
  tab, date(D), "[shape=record,label=\"{",
  style_date_record(D),
  "}\",style=filled,color=green,fillcolor=gray90];\n".

style_date_record(year(Y)) --> "Year|", format_("~s", [Y]).
style_date_record(date(D)) --> "Date|", format_("~s", [D]).

date(year(Y)) --> "\"", format_("~s", [Y]), "\"".
date(date(D)) --> "\"", format_("~s", [D]), "\"".

tab --> "    ".
