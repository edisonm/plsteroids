:- module(plsdirs, []).

:- use_module(library(apply)).

:- [plsconfig].
:- [plsloader].

:- packages(Packs),
   maplist(pack_set_path, Packs).
