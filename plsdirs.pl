:- module(plsdirs, []).

:- [plsconfig].
:- [plsloader].

:- packages(Packs),
   scanpacks(Packs, pack_set_path).
