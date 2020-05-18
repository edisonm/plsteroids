:- module(plsdirs, []).

:- [plsconfig].
:- [plsloader].

:- packages(Packs),
   maplist(pack_set_path, Packs).
