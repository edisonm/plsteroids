:- [plsteroids].
:- [plsconfig].
:- [loadpacks].
:- use_module(library(statistics)).
:- use_module(library(crypto)).
:- use_module(library(ssl)).
:- use_module(library(pldoc), []).
:- use_module(library(apply_macros), []).
:- if(\+ current_prolog_flag(processor, 'aarch64-linux')).
:- use_module(library(bid_eval), []).
:- endif.

:- set_prolog_flag(autoload, false).

:- loadpacks(use_module_ne).

:- set_prolog_flag(autoload, true).

:- [library(stchecks)].
:- [library(ws_source)].

% Check that we don't load the compound expand operator in the user's space     
:- \+ current_op(1, fx, user:'$compound_expand').
