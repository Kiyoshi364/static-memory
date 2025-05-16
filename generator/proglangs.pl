:- module(proglangs, [proglang/1, proglang_val/2]).

proglang(PL) :- proglang_val(PL, _).

proglang_val(apl        , name_link('APL', https('aplwiki.com/'))).
proglang_val(c          , name_link('C', https('en.wikipedia.org/wiki/C_(programming_language)'))).
proglang_val(coq        , name_link('Coq', https('coq.inria.fr/'))).
proglang_val(elixir     , name_link('Elixir', https('elixir-lang.org/'))).
proglang_val(elm        , name_link('Elm', https('elm-lang.org/'))).
proglang_val(erlang     , name_link('Erlang', https('www.erlang.org/'))).
proglang_val(haskell    , name_link('Haskell', https('www.haskell.org/'))).
proglang_val(idris      , name_link('Idris', https('www.idris-lang.org/'))).
proglang_val(j          , name_link('J', https('www.jsoftware.com/indexno.html'))).
proglang_val(prolog     , name_link('Prolog', https('en.wikipedia.org/wiki/Prolog'))).
proglang_val(python     , name_link('Python', https('www.python.org/'))).
proglang_val(roc        , name_link('Roc', https('www.roc-lang.org/'))).
proglang_val(wasm       , name_link('Wasm', https('webassembly.org/'))).
proglang_val(zig        , name_link('Zig', https('ziglang.org/'))).

proglang_val(latex      , name_link('LaTeX', https('www.latex-project.org/'))).
proglang_val(typst      , name_link('Typst', https('typst.app/'))).

proglang_val(git        , name_link('Git', https('git-scm.com/'))).
proglang_val(nix        , name_link('Nix', https('nixos.org/'))).
