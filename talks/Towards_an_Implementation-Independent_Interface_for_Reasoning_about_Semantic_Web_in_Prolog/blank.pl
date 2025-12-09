influenced('Prolog', blank('Erlang')).
influenced(blank('Erlang'), 'Clojure').
influenced('Prolog', 'Clojure').

first_appeared(_, _) :- false.
