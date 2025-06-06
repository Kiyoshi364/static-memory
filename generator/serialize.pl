:- module(serialize, [
  serialize_header//1, serialize_body//2
]).

:- use_module(library(lists), [member/2, foldl/4]).
:- use_module(library(dcgs), [phrase/3, seq//1]).

:- use_module(proglangs, [proglang_val/2]).
:- use_module(me, [mygithub/1, mygitlab/1]).

serialize_header(H) -->
  serialize_header_names(H), serialize_header_align(H).

serialize_header_names(H) -->
  foldl(serialize_header_name_, H), "|\n".

serialize_header_name_(Name) --> "|", seq(Name).

serialize_header_align(H) -->
  foldl(serialize_header_align_, H), "|\n".

serialize_header_align_(_) --> "|:---:".

serialize_body(Ts, Func) -->
  { functor(Func, _, Arity) },
  serialize_body_(Ts, 0, Arity, Func),
  "|\n",
[].

serialize_body_([], Arity, Arity, _) --> [].
serialize_body_([T | Ts], N, Arity, Func) -->
  { N < Arity,
    N1 is N+1, arg(N1, Func, Val)
  },
  "|", serialize_type_val(T, Val),
  serialize_body_(Ts, N1, Arity, Func).

serialize_type_val(text, literal(L)) --> !, seq(L).
serialize_type_val(date, year_month(Y, M)) --> !, serialize_number(Y), "-", serialize_month(M).
serialize_type_val(link, Val) --> !,
  ( { Val = name_link(N, L) } -> "[", seq(N), "](", serialize_linktarget(L), ")"
  ; { Val = doi(ID)         } -> "[DOI(", seq(ID), ")](", serialize_linktarget(doi(ID)), ")"
  ; { Val = mygithub(Path)  } -> { mygithub(GITHUB) }, "[", seq(GITHUB), "/", seq(Path), "](", serialize_linktarget(mygithub(Path)), ")"
  ; { Val = mygitlab(Path)  } -> { mygitlab(GITLAB) }, "[", seq(GITLAB), "/", seq(Path), "](", serialize_linktarget(mygitlab(Path)), ")"
  ; { throw(unknown_link_while_serializing(Val)) }
  ).
serialize_type_val(proglang, proglang(PL)) --> !, { proglang_val(PL, Val) }, serialize_type_val(link, Val).
serialize_type_val(list(T, J, E, N), L) --> !, serialize_list(L, T, J, E, N).
serialize_type_val(or(Ts), O) --> !, serialize_or(Ts, O).
serialize_type_val(_, to_be_filled) --> !, "???".
serialize_type_val(T, Val) -->
  { throw(unknown_type_val_while_serializing(T, Val)) }.

serialize_number(N) --> { number_chars(N, Cs) }, seq(Cs).
serialize_month(M) --> ( { M < 10 } -> "0" ; [] ), serialize_number(M).

serialize_linktarget(publications(L)) --> !, "./publications/", seq(L).
serialize_linktarget(https(L)) --> !, "https://", seq(L).
serialize_linktarget(http(L)) --> !, "http://", seq(L).
serialize_linktarget(doi(ID)) --> !, "https://doi.org/", seq(ID).
serialize_linktarget(mygithub(Path)) --> !, { mygithub(GITHUB) }, "https://", seq(GITHUB), "/", seq(Path).
serialize_linktarget(mygitlab(Path)) --> !, { mygitlab(GITLAB) }, "https://", seq(GITLAB), "/", seq(Path).
serialize_linktarget(Link) -->
  { throw(unknown_link_while_serializing(Link)) }.

serialize_list([], _, _, _, None) --> seq(None).
serialize_list([Val0 | Vals], T, Join, End, _) --> serialize_list_(Vals, Val0, T, Join, End).

serialize_list_([], Val0, T, _, End) --> serialize_type_val(T, Val0), seq(End).
serialize_list_([Val1 | Vals], Val0, T, Join, End) -->
  serialize_type_val(T, Val0), seq(Join), serialize_list(Vals, Val1, T, Join, End).

serialize_or(Ts, O) -->
  ( { O = or(T, Val), member(T, Ts) } -> serialize_type_val(T, Val)
  ; { throw(unknown_or_while_serializing(Ts, Val)) }
  ).
