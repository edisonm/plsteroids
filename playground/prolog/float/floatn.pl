:- module(floatn, [mpfr_get_default_prec/1]).

:- use_module(library(assertions)).
:- use_module(library(plprops)).
:- use_module(library(float/evalexpr)).
:- use_module(library(foreign/foreign_interface)).
:- use_module(library(foreign/foreign_props)).
% :- extra_compiler_opts('-O2 -gdwarf-2 -g3 -lmpfr').
:- extra_compiler_opts('-lmpfr -lgmp').
:- use_foreign_header('pl-floatn').
:- use_foreign_source('pl-floatn').
:- gen_foreign_library(floatn).

/*

Multiple Precision Floating-Point Reliable Library Prolog Interface.

We name it floatn in to avoid clashes with names derived from mpfr in the C
side.

*/

:- type floatn/1 + foreign(is_floatn).
:- type mpfr_prec_t/1 + foreign(is_mpfr_prec_t).

% documentation at /usr/share/doc/libmpfr-doc/mpfr.html

:- pred [ [ floatn_new_value(+term_t, int, -term_t),
            floatn_init
          ] + native,
          mpfr_get_default_prec(-Prec:mpfr_prec_t)+(returns(Prec), foreign),
          
          log2(   int, -floatn) + native(floatn_const_log2),
          pi(     int, -floatn) + native(floatn_const_pi),
          euler(  int, -floatn) + native(floatn_const_euler),
          catalan(int, -floatn) + native(floatn_const_catalan),
          sqrt(   +floatn, int, -floatn) + native(floatn_sqrt),
          cbrt(   +floatn, int, -floatn) + native(floatn_cbrt),
          -(      +floatn, int, -floatn) + native(floatn_neg),
          log(    +floatn, int, -floatn) + native(floatn_log),
          log2(   +floatn, int, -floatn) + native(floatn_log2),
          log10(  +floatn, int, -floatn) + native(floatn_log10),
          log1p(  +floatn, int, -floatn) + native(floatn_log1p),
          exp(    +floatn, int, -floatn) + native(floatn_exp),
          exp2(   +floatn, int, -floatn) + native(floatn_exp2),
          exp10(  +floatn, int, -floatn) + native(floatn_exp10),
          expm1(  +floatn, int, -floatn) + native(floatn_expm1),
          cos(    +floatn, int, -floatn) + native(floatn_cos),
          sin(    +floatn, int, -floatn) + native(floatn_sin),
          tan(    +floatn, int, -floatn) + native(floatn_tan),
          sec(    +floatn, int, -floatn) + native(floatn_sec),
          csc(    +floatn, int, -floatn) + native(floatn_csc),
          cot(    +floatn, int, -floatn) + native(floatn_cot),
          acos(   +floatn, int, -floatn) + native(floatn_acos),
          asin(   +floatn, int, -floatn) + native(floatn_asin),
          atan(   +floatn, int, -floatn) + native(floatn_atan),
          cosh(   +floatn, int, -floatn) + native(floatn_cosh),
          sinh(   +floatn, int, -floatn) + native(floatn_sinh),
          tanh(   +floatn, int, -floatn) + native(floatn_tanh),
          sech(   +floatn, int, -floatn) + native(floatn_sech),
          csch(   +floatn, int, -floatn) + native(floatn_csch),
          coth(   +floatn, int, -floatn) + native(floatn_coth),
          acosh(  +floatn, int, -floatn) + native(floatn_acosh),
          asinh(  +floatn, int, -floatn) + native(floatn_asinh),
          atanh(  +floatn, int, -floatn) + native(floatn_atanh),
          fac_ui(+int, int, -floatn) + native(floatn_fac_ui),
          eint(   +floatn, int, -floatn) + native(floatn_eint),
          li2(    +floatn, int, -floatn) + native(floatn_li2),
          gamma(  +floatn, int, -floatn) + native(floatn_gamma),
          lgamma( +floatn, int, -floatn) + native(floatn_lngamma),
          digamma(+floatn, int, -floatn) + native(floatn_digamma),
          zeta(   +floatn, int, -floatn) + native(floatn_zeta),
          erf(    +floatn, int, -floatn) + native(floatn_erf),
          erfc(   +floatn, int, -floatn) + native(floatn_erfc),
          j0(     +floatn, int, -floatn) + native(floatn_j0),
          j1(     +floatn, int, -floatn) + native(floatn_j1),
          y0(     +floatn, int, -floatn) + native(floatn_y0),
          y1(     +floatn, int, -floatn) + native(floatn_y1),
          ai(     +floatn, int, -floatn) + native(floatn_ai),
          +(    +floatn, +floatn, int, -floatn) + native(floatn_add),
          *(    +floatn, +floatn, int, -floatn) + native(floatn_mul),
          -(    +floatn, +floatn, int, -floatn) + native(floatn_sub),
          ^(    +floatn, +floatn, int, -floatn) + native(floatn_pow),
          **(   +floatn, +floatn, int, -floatn) + native(floatn_pow),
          div(  +floatn, +floatn, int, -floatn) + native(floatn_div),
          atan( +floatn, +floatn, int, -floatn) + native(floatn_atan2),
          atan2(+floatn, +floatn, int, -floatn) + native(floatn_atan2),
          beta( +floatn, +floatn, int, -floatn) + native(floatn_beta),
          agm(  +floatn, +floatn, int, -floatn) + native(floatn_agm),
          rootn_ui(+floatn, +int, int, -floatn) + native(floatn_rootn_ui),
          jn(+int, +floatn, int, -floatn) + native(floatn_jn),
          yn(+int, +floatn, int, -floatn) + native(floatn_yn),
          gamma_inc(+floatn, +floatn, int, -floatn) + native(floatn_gamma_inc),
          fma( +floatn, +floatn, +floatn, int, -floatn) + native(floatn_fma),
          fms( +floatn, +floatn, +floatn, int, -floatn) + native(floatn_fms),
          fmma(+floatn, +floatn, +floatn, +floatn, int, -floatn) + native(floatn_fmma),
          fmms(+floatn, +floatn, +floatn, +floatn, int, -floatn) + native(floatn_fmms)
        ].

:- initialization(floatn_init).

epsilon(P, V) :-
    ( var(P)
    ->mpfr_get_default_prec(N)
    ; N=P
    ),
    N1 is 1-N,
    evalexpr(floatn(P), 2^N1, V).

e(P, V) :- evalexpr(floatn(P), exp(1), V).

cputime(P, V) :-
    X is cputime,
    evalexpr(floatn(P), X, V).

eval(E, _, E).

+(E, _, E).

evalexpr:evaluable(floatn, E) :- evalexpr:evaluable(floatn(_), E).
evalexpr:new_value(floatn, E, V) :- evalexpr:new_value(floatn(_), E, V).
evalexpr:eval_func(floatn, F, V) :- evalexpr:eval_func(floatn(_), F, V).
evalexpr:eval_hook(floatn, E, V) :- evalexpr:eval_hook(floatn(_), E, V).

evalexpr:evaluable(floatn(_), E) :- evalexpr_evaluable(E).
evalexpr:new_value(floatn(P), E, V) :- floatn_new_value(E, P, V).
evalexpr:eval_func(floatn(P), F, V) :- eval_func_floatn(F, P, V).
evalexpr:eval_hook(floatn(P), E, V) :- evalexpr_floatn(E, P, V).

evalexpr_evaluable(log2).
evalexpr_evaluable(euler).
evalexpr_evaluable(catalan).
evalexpr_evaluable(agm(_, _)).
evalexpr_evaluable(expm1(_)).
evalexpr_evaluable(ai(_)).
evalexpr_evaluable(j0(_)).
evalexpr_evaluable(j1(_)).
evalexpr_evaluable(y0(_)).
evalexpr_evaluable(y1(_)).
evalexpr_evaluable(cbrt(_)).
evalexpr_evaluable(eint(_)).
evalexpr_evaluable(log2(_)).
evalexpr_evaluable(log1p(_)).
evalexpr_evaluable(zeta(_)).
evalexpr_evaluable(li2(_)).
evalexpr_evaluable(gamma(_)).
evalexpr_evaluable(digamma(_)).
evalexpr_evaluable(fma(_, _, _)).
evalexpr_evaluable(fms(_, _, _)).
evalexpr_evaluable(fmma(_, _, _, _)).
evalexpr_evaluable(fmms(_, _, _, _)).

eval_func_floatn(A/B, P, V) :-
    div(A, B, P, V),
    !.
eval_func_floatn(F, P, V) :- call(F, P, V).

evalexpr_floatn(floatn(E, P), _, V) :- evalexpr(floatn(P), E, V).
evalexpr_floatn(root(  E, N), P, V) :- evalexpr(floatn(P), E**(1/N), V).
evalexpr_floatn(rootn(E1, N), P, V) :-
    N1 is N,
    ( N >= 0
    ->E = E1
    ; E = 1/E1
    ),
    evalexpr(floatn(P), E, V1),
    rootn_ui(V1, N1, P, V).
evalexpr_floatn(fac_ui(E1), P, V) :-
    E is E1,
    fac_ui(E, P, V).
