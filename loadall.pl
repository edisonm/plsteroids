:- [plsteroids].
:- use_module(library(crypto)).
:- use_module(library(ssl)).
:- use_module(library(float/floatn)).
:- use_module(library(float/complexn)).
:- use_module(pldoc(doc_modes)).
:- [library(checkers)].
:- [loadpacks].

:- set_prolog_flag(autoload, false).

use_module_ne(Lib) :- use_module(Lib, []).

:- loadpacks(use_module_ne).
:- set_prolog_flag(autoload, true).
:- [library(ws_source)].
