:- module(spec_n,
          [compare_d/4]).

:- use_module(library(near_utils)).

:- multifile
        clpcd_highlight:clpcd_module/1,
        prolog_colour:syntax_message//1.

% Predicates specific to a domain

compare_d(clpn, Op, A, B) :-
    near_compare(Op, A, B).

clpcd_highlight:clpcd_module(clpn).

prolog_colour:syntax_message(constraint(clpcd(Sub))) -->
	[ 'clp(~w) constraint'-[Sub] ].
prolog_colour:syntax_message(type_error(constraint(clpcd(Sub)))) -->
	[ 'Only clp(~w) constraints may appear inside {}'-[Sub] ].
