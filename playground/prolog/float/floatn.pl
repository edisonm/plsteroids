:- module(floatn,
          [ mpfr_get_default_prec/1,
            floatn/1,
            floatn_data/1,
            floatn_domain_args/1,
            floatn_new_value/3,
            floatn_evalfunc/3
          ]).

:- use_module(library(lists)).
:- use_module(library(neck)).
:- use_module(library(assertions)).
:- use_module(library(substitute)).
:- use_module(library(extend_args)).
:- use_module(library(plprops)).
:- use_module(library(float/evaluator)).
:- use_module(library(float/float_props)).
:- use_module(library(foreign/foreign_generator)).
:- use_module(library(foreign/foreign_interface)).
:- use_module(library(foreign/foreign_props)).
% :- extra_compiler_opts('-O2 -gdwarf-2 -g3 -lmpfr').
:- extra_compiler_opts('-lmpfr -lgmp').
:- use_foreign_header('pl-floatn').
:- use_foreign_source('pl-floatn').
:- gen_foreign_library(plbin(libfloatn), floatn_init).

:- multifile
    floatn_evalfunc/3,
    floatn_domain_args/1.

/*

Multiple Precision Floating-Point Reliable Library Prolog Interface.

We name it floatn in to avoid clashes with names derived from mpfr in the C
side.

Note: install libmpfr first, for instance, in Debian:
apt install libmpfr-dev libmpc-dev

*/

:- type floatn/1 + native(is_floatn).

:- type mpfr_prec_t/1 + foreign(is_mpfr_prec_t).

% documentation at /usr/share/doc/libmpfr-doc/mpfr.html

:- pred [ [ floatn_new_value(+term, int, -term),
            floatn_data(-ptr)
          ] + native,
          [ mpfr_get_default_prec(-Prec:mpfr_prec_t)
          ] + (returns(Prec), foreign),
          [ [ log2/2, pi/2, euler/2, catalan/2
            ] :: (int * -floatn) + native(prefix(floatn_const_)),
            [ [ sqrt/3,  cbrt/3,    log/3,     log2/3,  log10/3, log1p/3, exp/3,   exp2/3,  exp10/3, expm1/3,
                cos/3,   sin/3,     tan/3,     sec/3,   csc/3,   cot/3,   acos/3,  asin/3,  atan/3,  cosh/3,
                sinh/3,  tanh/3,    sech/3,    csch/3,  coth/3,  acosh/3, asinh/3, atanh/3, eint/3,  li2/3,
                gamma/3, lngamma/3, digamma/3, zeta/3,  erf/3,   erfc/3,  j0/3,    j1/3,    y0/3,    y1/3,
                ai/3,    neg/3
              ] :: (+floatn * int * -floatn),
              [ add/4, mul/4, sub/4, pow/4, div/4, atan2/4, gamma_inc/4, beta/4, agm/4
              ] :: (+floatn * +floatn * int * -floatn),
              [jn/4,   yn/4  ] :: (+int * +floatn * int * -floatn),
              [fma/5,  fms/5 ] :: (+floatn * +floatn * +floatn * int * -floatn),
              [fmma/6, fmms/6] :: (+floatn * +floatn * +floatn * +floatn * int * -floatn),
              rootn_ui(+floatn, +int, int, -floatn)
              % fac_ui(+int, int, -floatn) % can not be casted in this framework
            ] + native(prefix(floatn_)),
            %% Non native:
            [ e/2, epsilon/2, cputime/2 ] :: (int * -floatn),
            [ eval/3, (+)/3, (-)/3, lgamma/3
            ] :: (+floatn * int * -floatn),
            [ (/)/4, (+)/4, (*)/4, (-)/4, (**)/4, (^)/4, atan/4, root/4
            ] :: (+floatn * +floatn * int * -floatn)
          ] + evaluable
        ].

epsilon(P, V) :-
    ( var(P)
    ->mpfr_get_default_prec(N)
    ; N=P
    ),
    N1 is 1-N,
    evalexpr(floatn(P), 2^N1, V).

cputime(P, V) :-
    X is cputime,
    evalexpr(floatn(P), X, V).

eval(E, _, E).
+(E, _, E).
-(A, P, V) :- neg(A, P, V).

/( A, B, P, V) :- div(A, B, P, V).
+( A, B, P, V) :- add(A, B, P, V).
*( A, B, P, V) :- mul(A, B, P, V).
-( A, B, P, V) :- sub(A, B, P, V).
**(A, B, P, V) :- pow(A, B, P, V).
^( A, B, P, V) :- pow(A, B, P, V).
lgamma(A, P, V) :- lngamma(A, P, V).
atan(A, B, P, V) :- atan2(A, B, P, V).
e(P, V) :- evalexpr(floatn(P), exp(1), V).
root(E, N, P, V) :- evalexpr(floatn(P), E**(1/N), V).

:- public
    curr_floatn_evaluable/2.

curr_floatn_evaluable(Func, InputDomains) :-
    curr_prop_asr(head, floatn:Call, _, Asr),
    curr_prop_asr(glob, floatn:evaluable(_), _, Asr),
    extend_args(Func, [_, _], Call),
    collect_prop(Asr, floatn, call, InputDomains1),
    subtract(InputDomains1, [var(_)], InputDomains).

floatn_evaluable(Func, InputDomains) :-
    curr_floatn_evaluable(Func, InputDomains),
    neck.

floatn_evalfunc(F, P, V) :-
    floatn_evaluable(F, _),
    extend_args(F, [P, V], C),
    neck,
    C.

:- public
    curr_floatn_domain_args/1.

curr_floatn_domain_args(D) :-
    floatn_evaluable(F, L1),
    maplist(domain_substitution, L1, L),
    substitute_values(L, F, D).

floatn_domain_args(D) :-
    curr_floatn_domain_args(D),
    neck.
