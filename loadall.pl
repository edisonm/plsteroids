:- [plsteroids].
:- [plsconfig].
:- [loadpacks].
:- use_module(library(statistics)).
:- use_module(library(crypto)).
:- use_module(library(ssl)).
:- use_module(library(pldoc), []).
:- use_module(library(apply_macros), []).
:- use_module(library(bid_eval), []).

:- set_prolog_flag(autoload, false).
use_module_ne(Lib) :- use_module(Lib, []).
:- loadpacks(use_module_ne).
:- set_prolog_flag(autoload, true).

:- [library(stchecks)].
:- [library(ws_source)].
