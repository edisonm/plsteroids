:- [plsteroids].
:- use_module(library(apply_macros)). % Load it proactively
:- use_module(pldoc(doc_modes)).
:- use_module(library(filesex)).
:- [library(stchecks)].
:- [loadpacks].

:- set_prolog_flag(autoload, false).
:- loadpacks(qcompile).
:- set_prolog_flag(autoload, true).
:- qcompile(library(ws_source)).
