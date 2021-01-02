:- module(clpcd_domain_ops,
          [active_clpcd/1,
           cast_d/3,
           clpcd_module/1,
           compare_d/4,
           div_d/4,
           eval_d/3,
           ceiling_d/3,
           floor_d/3,
           integerp/3,
           numbers_only/2,
           set_clpcd/1]).

:- multifile
        cast_d/3,
        compare_d/4,
        div_d/4,
        ceiling_d/3,
        floor_d/3,
        eval_d/3,
        integerp/3,
        numbers_only/2,
        clpcd_module/2.

:- dynamic
        active_clpcd/1.

set_clpcd(C) :-
    retractall(active_clpcd(_)),
    assertz(active_clpcd(C)).

clpcd_module(Module) :-
    active_clpcd(CD),
    clpcd_module(CD, Module).

