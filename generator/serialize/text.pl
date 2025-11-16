:- module(text,
[ lowercase/2
]).

:- use_module(library(lists), [maplist/3]).

lowercase(Upper, Lower) :-
  maplist(lowercase1, Upper, Lower).

lowercase1(A0, A) :- ( lowercase_(A0, A) -> true ; A0 = A ).

lowercase_('A', a).
lowercase_('B', b).
lowercase_('C', c).
lowercase_('D', d).
lowercase_('E', e).
lowercase_('F', f).
lowercase_('G', g).
lowercase_('H', h).
lowercase_('I', i).
lowercase_('J', j).
lowercase_('K', k).
lowercase_('L', l).
lowercase_('M', m).
lowercase_('N', n).
lowercase_('O', o).
lowercase_('P', p).
lowercase_('Q', q).
lowercase_('R', r).
lowercase_('S', s).
lowercase_('T', t).
lowercase_('U', u).
lowercase_('V', v).
lowercase_('W', w).
lowercase_('X', x).
lowercase_('Y', y).
lowercase_('Z', z).
