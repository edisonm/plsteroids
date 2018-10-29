:- module(clpcd, []).

:- use_module(library(clpcd/dump)).

		 /*******************************
		 *	 TOPLEVEL PRINTING	*
		 *******************************/

:- multifile
	prolog:message/3,
        prolog_colour:syntax_message//1.

prolog_colour:syntax_message(constraint(clpcd(Sub))) -->
	[ 'clp(~w) constraint'-[Sub] ].
prolog_colour:syntax_message(type_error(constraint(clpcd(Sub)))) -->
	[ 'Only clp(~w) constraints may appear inside {}'-[Sub] ].

prolog:message(query(YesNo,Bindings)) --> !,
	{dump_toplevel_bindings(Bindings,Constraints)},
	dump_format(Constraints),
        '$messages':prolog_message(query(YesNo,Bindings)).

dump_toplevel_bindings(Bindings,Constraints) :-
	dump_vars_names(Bindings,[],Vars,Names),
	dump(Vars,Names,Constraints).

dump_vars_names([],_,[],[]).
dump_vars_names([Name=Term|Rest],Seen,Vars,Names) :-
	(   var(Term),
	    (   get_attr(Term,clpcd_itf,_)
	    ;   get_attr(Term,clpcd_geler,_)
	    ),
	    \+ memberchk_eq(Term,Seen)
	->  Vars = [Term|RVars],
	    Names = [Name|RNames],
	    NSeen = [Term|Seen]
	;   Vars = RVars,
	    Names = RNames,
	    Seen = NSeen
	),
	dump_vars_names(Rest,NSeen,RVars,RNames).

dump_format([]) --> [].
dump_format([X|Xs]) -->
	['{~w}'-[X], nl],
	dump_format(Xs).

memberchk_eq(X,[Y|Ys]) :-
	(   X == Y
	->  true
	;   memberchk_eq(X,Ys)
	).
