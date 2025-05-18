:- set_prolog_flag(encoding, utf8).
:- use_module(xtools/prolog/record_locations, []). % must be the first
:- set_prolog_flag(autoload, false).
:- [packages].
:- [plsdirs].
:- [pltools].
:- set_prolog_flag(autoload, true).

% Check that we don't load the compound expand operator in the user's space     
:- \+ current_op(1, fx, user:'$compound_expand').
