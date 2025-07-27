:- module(proglangs, [proglang/1, proglang_val/2]).

proglang(PL) :- proglang_val(PL, _).

proglang_val(apl        , text_link("APL", https("aplwiki.com/"))).
proglang_val(c          , text_link("C", https("en.wikipedia.org/wiki/C_(programming_language)"))).
proglang_val(coq        , text_link("Coq", https("coq.inria.fr/"))).
proglang_val(elixir     , text_link("Elixir", https("elixir-lang.org/"))).
proglang_val(elm        , text_link("Elm", https("elm-lang.org/"))).
proglang_val(erlang     , text_link("Erlang", https("www.erlang.org/"))).
proglang_val(haskell    , text_link("Haskell", https("www.haskell.org/"))).
proglang_val(idris      , text_link("Idris", https("www.idris-lang.org/"))).
proglang_val(j          , text_link("J", https("www.jsoftware.com/indexno.html"))).
proglang_val(prolog     , text_link("Prolog", https("en.wikipedia.org/wiki/Prolog"))).
proglang_val(python     , text_link("Python", https("www.python.org/"))).
proglang_val(roc        , text_link("Roc", https("www.roc-lang.org/"))).
proglang_val(wasm       , text_link("Wasm", https("webassembly.org/"))).
proglang_val(zig        , text_link("Zig", https("ziglang.org/"))).

proglang_val(latex      , text_link("LaTeX", https("www.latex-project.org/"))).
proglang_val(typst      , text_link("Typst", https("typst.app/"))).

proglang_val(git        , text_link("Git", https("git-scm.com/"))).
proglang_val(nix        , text_link("Nix", https("nixos.org/"))).
