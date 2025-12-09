:- module(typst,
[ serialize_typst_named_listof_dictionary//3
]).

:- use_module(library(lists), [member/2]).
:- use_module(library(dcgs), [seq//1]).

:- use_module(serialize,
[ link_normalized/3, proglang_normalized/3
, serialize_atom//1
, serialize_number//1, serialize_month//1, serialize_day//1
, nlindent//1
, ntfoldl//2
, ntfoldlf//3
]).

serialize_typst_named_listof_dictionary(Name, T, Bs) -->
  "#let ", serialize_atom(Name), " = ",
  serialize_typst_listof_dictionary(1, T, Bs),
  ";\n",
[].

serialize_typst_listof_dictionary(Indent, T, Bs) -->
  { Indent_1 is Indent - 1 },
  "(", nlindent(Indent),
  ntfoldl(serialize_typst_listof_dictionary_(Indent, T), Bs),
  nlindent(Indent_1), ")",
[].

serialize_typst_listof_dictionary_(Indent, T, B) -->
  serialize_typst_dictionary(Indent, T, B),
  ", ",
[].

serialize_typst_dictionary(Indent, T, B) -->
  { Indent1 is Indent + 1 },
  "(",
  serialize_typst_dictionary_(Indent1, T, B),
  nlindent(Indent), ")",
[].

serialize_typst_dictionary_(Indent, T, B) -->
  ntfoldlf(serialize_typst_dictionary_key_value(Indent), T, B).

serialize_typst_dictionary_key_value(Indent, field(K, T), Val) -->
  nlindent(Indent),
  serialize_atom(K), ": ",
  serialize_type_val(T, Val), ",",
[].

serialize_type_val(text, literal(L)) --> !, quoted(L).
serialize_type_val(date, Date) --> !, serialize_date(Date).
serialize_type_val(link, Val) --> !, { link_normalized(Val, Text, Link) }, serialize_link(Text, Link).
serialize_type_val(proglang, proglang(PL)) --> !, { proglang_normalized(PL, Text, Link) }, serialize_link(Text, Link).
serialize_type_val(list(T, _, _, _), L) --> !, serialize_list(L, T).
serialize_type_val(or(Cs), O) --> !, serialize_or(Cs, O).
serialize_type_val(_, to_be_filled) --> !, "text[To Be Filled]".
serialize_type_val(T, Val) -->
  { throw(unknown_type_val_while_serializing_typst(T, Val)) }.

serialize_date(year_month(Y, M)) --> "date(year: ", serialize_number(Y), ", month: ", serialize_month(M), ")".
serialize_date(year_month_day(Y, M, D)) --> "date(year: ", serialize_number(Y), ", month: ", serialize_month(M), ", day: ", serialize_day(D), ")".

serialize_link(Text, Link) --> "link(\"", serialize_linktarget(Link), "\")[", seq(Text), "]".

serialize_linktarget(publications(L)) --> !, "file://./publications/", seq(L).
serialize_linktarget(talks(L)) --> !, "file://./talks/", seq(L).
serialize_linktarget(external(L)) --> !, seq(L).
serialize_linktarget(Link) -->
  { throw(unknown_linktarget_while_serializing_typst(Link)) }.

serialize_list([], _) --> "()".
serialize_list([Val | Vals], T) -->
  "( ",
  serialize_list_(T, Val),
  ntfoldl(serialize_list_(T), Vals),
  ")".

serialize_list_(T, Val) --> serialize_type_val(T, Val), ", ".

serialize_or(Cs, O) -->
  ( { O = or(Tag, Val), member(case(Tag, T), Cs) } ->
    { atom_chars(Tag, Tagchs), atom_chars(T, Tchs) },
    "(",
    " tag: ", quoted(Tagchs), ",",
    " type: ", quoted(Tchs), ",",
    " val: ", serialize_type_val(T, Val), ",",
    ")"
  ; { throw(unknown_or_while_serializing_typst(Cs, Val)) }
  ).
 
 quoted(Cs) --> "\"", seq(Cs), "\"".
