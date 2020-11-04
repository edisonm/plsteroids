
:- export(
    ({}/1,
    maximize/1,
    minimize/1,
    inf/2,
    inf/4,
    sup/2,
    sup/4,
    bb_inf/3,
    bb_inf/4,
    entailed/1)).

:- use_module(library(neck)).
:- use_module(library(clpcd/bb)).
:- use_module(library(clpcd/nf)).
:- use_module(library(clpcd/bv)).
:- reexport(library(clpcd/dump),
            [ dump/3 %, projecting_assert/1
            ]).
:- reexport(library(clpcd/ordering), [clp_type/2]).
:- use_module(library(clpcd)).
:- reexport(library(clpcd/ordering), [ordering/1]).

clpcd_highlight:clpcd_module(D) :-
    '$current_source_module'(D),
    neck.

inf(Expression, Inf) :-
    '$current_source_module'(D),
    neck,
    inf(D, Expression, Inf).

inf(Expression, Inf, Vector, Vertex) :-
    '$current_source_module'(D),
    neck,
    inf(D, Expression, Inf, Vector, Vertex).

sup(Expression, Sup) :-
    '$current_source_module'(D),
    neck,
    sup(D, Expression, Sup).

sup(Expression, Sup, Vector, Vertex) :-
    '$current_source_module'(D),
    neck,
    sup(D, Expression, Sup, Vector, Vertex).

maximize(Term) :-
    '$current_source_module'(D),
    neck,
    maximize(D, Term).

minimize(Term) :-
    '$current_source_module'(D),
    neck,
    minimize(D, Term).

{Rel} :-
    '$current_source_module'(D),
    neck,
    add_constraint(Rel, D).

entailed(C) :-
    '$current_source_module'(D),
    neck,
    entailed(D, C).

bb_inf(Is, Term, Inf) :-
    '$current_source_module'(D),
    neck,
    bb_inf(D, Is, Term, Inf, _).

bb_inf(Is, Term, Inf, Vertex) :-
    '$current_source_module'(D),
    neck,
    bb_inf(D, Is, Term, Inf, Vertex).

		 /*******************************
		 *	       SANDBOX		*
		 *******************************/
:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(D:H) :-
    '$current_source_module'(D),
    member(H, [{_},
               entailed(_),
               bb_inf(_, _, _),
               bb_inf(_, _, _, _),
               maximize(_),
               minimize(_),
               inf(_,_),
               inf(_,_,_,_),
               sup(_,_),
               sup(_,_,_,_)
              ]),
    neck.
