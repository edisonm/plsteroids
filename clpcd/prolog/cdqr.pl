:- module(cdqr, []).

:- use_module(library(arithmetic)).
:- use_module(library(neck)).
:- use_module(library(clpcd/domain_ops)).
:- use_module(library(clpcd/nf)).
:- use_module(library(clpcd)).
:- use_module(library(arithex)).

:- meta_predicate cdqr_eval_d(+,?).

cdqr_eval_d(F, R) :-
    arithmetic_expression_value(F, R).

/*
cdqr_eval_d(F, R) :-
    num_arithmetic_function(F),
    neck,
    R is F.
*/

:- public cdqr/1.

cdqr(cdq).
cdqr(cdr).

clpcd_domain_ops:eval_d(C, F, R) :-
    cdqr(C),
    neck,
    cdqr_eval_d(F, R).

clpcd_nf:nl_invertible(C,F) :-
    cdqr(C),
    neck,
    cd_invertible(F).

clpcd_nf:nl_invert(C,F,X,Y,Res) :-
    cdqr(C),
    neck,
    cd_invert(F,C,X,Y,N),
    cast_d(C,N,Res).

clpcd_nf:nonlin(C, Term, AL, Skel, SL) :-
    cdqr(C),
    neck,
    cd_nonlin(Term, AL, Skel, SL).
