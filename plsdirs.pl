:- module(plsdirs, []).

:- use_module(library(apply)).

:- [plsconfig].
:- [xlibrary/prolog/packloader].

:- forall(package(Pack), pack_set_path(Pack)).
