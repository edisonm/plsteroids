:- module(libcomplexn,
          [ complexn_t/1,
            complexn/4
          ]).

:- use_module(library(neck)).
:- use_module(library(assertions)).
:- use_module(library(lists)).
:- use_module(library(plprops)).
:- use_module(library(foreign/foreign_generator)).
:- use_module(library(foreign/foreign_interface)).
:- use_module(library(foreign/foreign_props)).
:- use_module(library(libfloatn), []).
% :- extra_compiler_opts('-O2 -gdwarf-2 -g3 -lmpfr').
:- extra_compiler_opts('-lmpc -lmpfr -lgmp').
:- include_foreign_dir('../../mpfr/prolog').
:- use_foreign_header('pl-complexn').
:- use_foreign_source('pl-complexn').
:- gen_foreign_library(plbin(libcomplexn)).
:- use_module(library(gen_complexn)).
:- gen_complexn.

:- type [ complexn_t/1
        ] + native(prefix(is_)).

:- pred [ [ complexn(+term, int, int, -term)
          ] + native(prefix(pl_))
          % [ [ eval/4, (+)/4, (-)/4, cbrt/4
          %   ] :: (+complexn * int * int * -complexn),
          %   [ i/3
          %   ] :: (int * int * -complexn),
          %   [ c/5 :: (+floatn * +floatn * int * int * -complexn)
          %   ] + native(complexn_set_fr_fr),
          %   [ (/)/5, (+)/5, (*)/5, (-)/5, (**)/5, (^)/5, root/5
          %   ] :: (+complexn * +complexn * int * int * -complexn)
          % ] + evaluable
        ].

:- include(plbin(complexn_auto)).

/*

:- use_module(library(substitute)).
:- use_module(library(extend_args)).

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
*/
