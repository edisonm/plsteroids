% load tools
:- use_module(library(comment_data)).
:- use_module(library(prolog_xref)). % Be careful: this after comment_data
:- use_module(library(metaprops)).
:- use_module(library(refactor)).
:- use_module(library(assrt_meta)).
:- use_module(library(stchecks)).
% :- use_module(library(ws_cover)).
:- use_module(library(i18n/i18n_op)).
/*
:- use_module(library(prolog_stack)).
:- multifile
	user:prolog_exception_hook/4.
:- dynamic
	user:prolog_exception_hook/4.

skip_error(missing(undecided_call)).
skip_error(error(type_error(Type, _), _)) :- member(Type, [integer, rational]).

:- dynamic prolog:history/2.

user:prolog_exception_hook(Error, _, _, _) :-
    \+ skip_error(Error),
    format(user_error, '~q~n', [Error]),
    backtrace(20),
    fail.
% user:prolog_trace_interception(Port, Frame, PC, continue).
*/
