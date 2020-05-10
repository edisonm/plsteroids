:- [plsconfig].
:- [loadpacks].
:- [packages].
:- use_module(library(crypto)).
:- use_module(library(ssl)).

:- set_prolog_flag(autoload, false).
use_module_ne(Lib) :- use_module(Lib, []).
:- loadpacks(use_module_ne).
:- set_prolog_flag(autoload, true).

:- [plsdirs].
:- [pltools].

:- [library(checkers)].
:- [library(ws_source)].
