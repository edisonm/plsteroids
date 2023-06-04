:- module(plsdirs, []).

:- use_module(library(apply)).

:- [plsconfig].
:- [xlibrary/prolog/packloader].

:- packages(Packs),
   maplist(pack_set_path, Packs).
