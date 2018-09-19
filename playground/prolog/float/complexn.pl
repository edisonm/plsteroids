:- module(complexn,
          [ complexn_new_value/4,
            complexn_evalfunc/4,
            complexn_domain_args/1]).

:- use_module(library(neck)).
:- use_module(library(assertions)).
:- use_module(library(substitute)).
:- use_module(library(extend_args)).
:- use_module(library(plprops)).
:- use_module(library(float/floatn)).
:- use_module(library(foreign/foreign_generator)).
:- use_module(library(foreign/foreign_interface)).
:- use_module(library(foreign/foreign_props)).
% :- extra_compiler_opts('-O2 -gdwarf-2 -g3 -lmpfr').
:- extra_compiler_opts('-lmpc -lgmp').
:- use_foreign_header('pl-complexn').
:- use_foreign_source('pl-complexn').
:- gen_foreign_library(libcomplexn).

:- type complexn/1 + native(is_complexn).

:- global evaluable/1.

evaluable(G) :- call(G).

:- pred [ [ complexn_new_value(+term_t, int, int, -term_t),
            complexn_bind_floatn(+ptr)
          ] + native,
          [ [ [ sqrt/4, neg/4, log/4, log10/4, exp/4, cos/4, sin/4, tan/4, acos/4, asin/4,
                atan/4, cosh/4, sinh/4, tanh/4, acosh/4, asinh/4, atanh/4, proj/4
              ] :: (+complexn * int * int * -complexn),
              [ add/5, mul/5, sub/5, div/5, pow/5
              ] :: (+complexn * +complexn * int * int * -complexn),
              [ arg/4
              ] :: (+complexn * int * int * -floatn)
            ] + native(prefix(complexn_)),
            [ eval/4, (+)/4, (-)/4, cbrt/4
            ] :: (+complexn * int * int * -complexn),
            [ (/)/5, (+)/5, (*)/5, (-)/5, (**)/5, (^)/5, root/5
            ] :: (+complexn * +complexn * int * int * -complexn)
          ] + evaluable
        ].

:- initialization(complexn_init).

eval(E, _, _, E).
+(E, _, _, E).
-(A, R, I, V) :- neg(A, R, I, V).
/( A, B, R, I, V) :- div(A, B, R, I, V).
+( A, B, R, I, V) :- add(A, B, R, I, V).
*( A, B, R, I, V) :- mul(A, B, R, I, V).
-( A, B, R, I, V) :- sub(A, B, R, I, V).
**(A, B, R, I, V) :- pow(A, B, R, I, V).
^( A, B, R, I, V) :- pow(A, B, R, I, V).
cbrt(E, R, I, V) :-    evalexpr(complexn(R, I), E**(1/3), V).
root(E, N, R, I, V) :- evalexpr(complexn(R, I), E**(1/N), V).

complexn_init :-
    floatn_data(P),
    complexn_bind_floatn(P).

complexn_evaluable(F, R, I, V, C, Asr) :-
    curr_prop_asr(head, complexn:C, _, Asr),
    curr_prop_asr(glob, complexn:evaluable(_), _, Asr),
    extend_args(F, [R, I, V], C).

domain_substitution(complexn(A), A=complexn(_, _)) :- !.
domain_substitution(floatn(A), A=floatn(_)) :- !.
domain_substitution(Prop1, A=Prop) :-
    extend_args(Prop, [A], Prop1).

complexn_evalfunc(Domain, F, R, I, V, C) :-
    complexn_evaluable(F, R, I, V, C, Asr),
    collect_prop(Asr, complexn, succ, Out1),
    member(Out, Out1),
    domain_substitution(Out, _=Domain).

complexn_evalfunc(F, R, I, V) :-
    complexn_evalfunc(complexn(_, _), F, R, I, V, C),
    neck,
    C.

floatn:floatn_evalfunc(E, P, V) :-
    complexn_evalfunc(floatn(_), E, P, P, V, C),
    neck,
    C.

complexn_domain_args(Domain, FDom) :-
    complexn_evaluable(Func, _, _, _, _, Asr),
    collect_prop(Asr, complexn, call, Inp1),
    subtract(Inp1, [var(_)], Inp2),
    maplist(domain_substitution, Inp2, Inp),
    substitute_values(Inp, Func, FDom),
    collect_prop(Asr, complexn, succ, Out1),
    member(Out, Out1),
    domain_substitution(Out, _=Domain).

complexn_domain_args(FDom) :-
    complexn_domain_args(complexn(_, _), FDom),
    neck.

floatn:floatn_domain_args(FDom) :- 
    complexn_domain_args(floatn(_), FDom),
    neck.
