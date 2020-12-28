:- module(libfloatn,
          [ mpfr_get_default_prec/1,
            floatn_t/1,
            floatn/3
          ]).

:- use_module(library(lists)).
:- use_module(library(neck)).
:- use_module(library(assertions)).
:- use_module(library(plprops)).
:- use_module(library(foreign/foreign_generator)).
:- use_module(library(foreign/foreign_interface)).
:- use_module(library(foreign/foreign_props)).
% :- extra_compiler_opts('-O2 -gdwarf-2 -g3 -lmpfr').
:- extra_compiler_opts('-lmpfr -lgmp').
:- use_foreign_header('pl-floatn').
:- use_foreign_source('pl-floatn').
:- gen_foreign_library(plbin(libfloatn), floatn_init).
:- use_module(library(gen_floatn)).
:- gen_floatn.

/*

Multiple Precision Floating-Point Reliable Library Prolog Interface.

We name it floatn to avoid clashes with names derived from mpfr in the C
side.

Note: install libmpfr first, for instance, in Debian:
apt install libmpfr-dev libmpc-dev

*/

:- type [ floatn_t/1,
          mpfr_prec_t/1
        ] + native(prefix(is_)).

% documentation at /usr/share/doc/libmpfr-doc/mpfr.html

:- pred [ [ floatn(+term, int, -term)
          ] + native(prefix(pl_)),
          [ mpfr_get_default_prec(-Prec:mpfr_prec_t)
          ] + (returns(Prec), foreign)
          % [ [ % fac_ui(+int, int, -floatn) % can not be casted in this framework
          %   ] + native(prefix(floatn_)),
          % ] + evaluable
        ].

:- include(plbin(floatn_auto)).

/*
:- use_module(library(substitute)).
:- use_module(library(extend_args)).
:- use_module(library(float_props)).

:- export floatn_domain_args/1,
          floatn_evalfunc/3.

:- multifile
    floatn_evalfunc/3,
    floatn_domain_args/1.

:- public
    curr_floatn_evaluable/2.

curr_floatn_evaluable(Func, InputDomains) :-
    curr_prop_asr(head, libfloatn:Call, _, Asr),
    curr_prop_asr(glob, libfloatn:evaluable(_), _, Asr),
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
*/
