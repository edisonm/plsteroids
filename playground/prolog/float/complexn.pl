:- module(complexn,
          [ complexn_new_value/4,
            complexn_evalfunc/4,
            complexn_domain_args/1]).

:- use_module(library(neck)).
:- use_module(library(assertions)).
:- use_module(library(substitute)).
:- use_module(library(extend_args)).
:- use_module(library(plprops)).
:- use_module(library(float/evaluator)).
:- use_module(library(float/floatn)).
:- use_module(library(float/float_props)).
:- use_module(library(foreign/foreign_generator)).
:- use_module(library(foreign/foreign_interface)).
:- use_module(library(foreign/foreign_props)).
% :- extra_compiler_opts('-O2 -gdwarf-2 -g3 -lmpfr').
:- extra_compiler_opts('-lmpc -lgmp').
:- use_foreign_header('pl-complexn').
:- use_foreign_source('pl-complexn').
:- gen_foreign_library(plbin(libcomplexn)).

:- type complexn/1 + native(is_complexn).

:- pred [ [ complexn_new_value(+term, int, int, -term)
          ] + native,
          [ [ [ sqrt/4, neg/4,  log/4,  log10/4, exp/4,  cos/4,    sin/4,   tan/4, acos/4, asin/4,
                atan/4, cosh/4, sinh/4, tanh/4,  acosh/4, asinh/4, atanh/4, proj/4
              ] :: (+complexn * int * int * -complexn),
              [ add/5, mul/5, sub/5, div/5, pow/5
              ] :: (+complexn * +complexn * int * int * -complexn),
              [ abs/4, arg/4, norm/4, real/4, imag/4
              ] :: (+complexn * int * int * -floatn)
            ] + native(prefix(complexn_)),
            [ eval/4, (+)/4, (-)/4, cbrt/4
            ] :: (+complexn * int * int * -complexn),
            [ i/3
            ] :: (int * int * -complexn),
            [ cnan/3 :: (int * int * -complexn)
            ] + native(complexn_set_nan),
            [ c/5 :: (+floatn * +floatn * int * int * -complexn)
            ] + native(complexn_set_fr_fr),
            [ (/)/5, (+)/5, (*)/5, (-)/5, (**)/5, (^)/5, root/5
            ] :: (+complexn * +complexn * int * int * -complexn)
          ] + evaluable
        ].

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
i(R, I, V) :- evalexpr(complexn(R, I), c(0,1), V).

:- public
        complexn_evalfunc/6,
        complexn_evaluable/6.

complexn_evaluable(F, R, I, V, C, Asr) :-
    curr_prop_asr(head, complexn:C, _, Asr),
    prop_asr(glob, evaluable(_), _, Asr),
    extend_args(F, [R, I, V], C).

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

:- public complexn_domain_args/2.

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
