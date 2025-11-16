:- module(md,
[ serialize_header//1, serialize_body//2
]).

:- use_module(library(lists), [member/2, foldl/4]).
:- use_module(library(dcgs), [seq//1]).

:- use_module(serialize,
[ link_normalized/3, proglang_normalized/3
, serialize_number//1, serialize_month//1
, foldlf/5
]).

serialize_header(H) -->
  serialize_header_names(H), serialize_header_align(H).

serialize_header_names(H) -->
  foldl(serialize_header_name_, H), "|\n".

serialize_header_name_(Name) --> "|", seq(Name).

serialize_header_align(H) -->
  foldl(serialize_header_align_, H), "|\n".

serialize_header_align_(_) --> "|:---:".

serialize_body(Fs, Func) -->
  foldlf(serialize_body_, Fs, Func),
  "|\n",
[].

serialize_body_(field(_, T), Val) -->
  "|", serialize_type_val(T, Val).

serialize_type_val(text, literal(L)) --> !, seq(L).
serialize_type_val(date, year_month(Y, M)) --> !, serialize_number(Y), "-", serialize_month(M).
serialize_type_val(link, Val) --> !, { link_normalized(Val, Text, Link) }, serialize_link(Text, Link).
serialize_type_val(proglang, proglang(PL)) --> !, { proglang_normalized(PL, Text, Link) }, serialize_link(Text, Link).
serialize_type_val(list(T, J, E, N), L) --> !, serialize_list(L, T, J, E, N).
serialize_type_val(or(Ts), O) --> !, serialize_or(Ts, O).
serialize_type_val(_, to_be_filled) --> !, "???".
serialize_type_val(T, Val) -->
  { throw(unknown_type_val_while_serializing(T, Val)) }.

serialize_link(Text, Link) --> "[", seq(Text), "](", serialize_linktarget(Link), ")".

serialize_linktarget(publications(L)) --> !, "./publications/", seq(L).
serialize_linktarget(external(L)) --> !, seq(L).
serialize_linktarget(Link) -->
  { throw(unknown_linktarget_while_serializing(Link)) }.

serialize_list([], _, _, _, None) --> seq(None).
serialize_list([Val0 | Vals], T, Join, End, _) --> serialize_list_(Vals, Val0, T, Join, End).

serialize_list_([], Val0, T, _, End) --> serialize_type_val(T, Val0), seq(End).
serialize_list_([Val1 | Vals], Val0, T, Join, End) -->
  serialize_type_val(T, Val0), seq(Join), serialize_list(Vals, Val1, T, Join, End).

serialize_or(Ts, O) -->
  ( { O = or(T, Val), member(T, Ts) } -> serialize_type_val(T, Val)
  ; { throw(unknown_or_while_serializing(Ts, Val)) }
  ).
