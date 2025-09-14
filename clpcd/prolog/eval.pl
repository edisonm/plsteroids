/*  Constraint logic programming over continuous domains

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2020, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- export(eepsilon/2).
:- export(eepsilon/3).
:- export(eval/3).
:- export(cast/3).
:- export(castable/2).
:- export(compare/4).
:- export(near_compare/4).

:- use_module(library(solution_sequences)).

:- public eval_1/4.

:- multifile check_dupcode:ignore_dupcode/3.

% TBD: Move the predicates refered in this clause to a module, instead of an included file
check_dupcode:ignore_dupcode(Head, Source, _) :-
    '$current_source_module'(Source),
    member(F/A, [eepsilon/2, eepsilon/3, eval/3, cast/3, castable/2, compare/4, near_compare/4,
                 compare/5, do_eval_cputime/2, do_eval_z/2, reserve_eps/1, near_compare_b/4]),
    functor(Head, F, A),
    neck.

eval_1(Type, Arg, eval(Type, Arg, EA), EA).

eval(_, Expr, _) :-
    var(Expr),
    !,
    fail.
eval(Type, Expr, C) :-
    do_eval(Expr, Type, C),
    !.
eval(Type, Value, C) :-
    cast(Type, Value, C),
    !.
eval(Type, Value, _) :-
    throw(error(type_error(evaluable, Type:Value), _)).

cast(Type, Value, C) :-
    ( inner_cast(Type, Value, C)
    ->true
    ; integer(Value)
    ->term_string(Value, String),
      cast(Type, String, C)
    ; rational(Value)
    ->X is numerator(Value),
      Y is denominator(Value),
      do_eval(X/Y, Type, C)
    ; ground(Value)
    ->do_eval(Value, Type, C)
    ).

castable(Type, Value) :-
    cd_prefix(Type, Pref, _),
    atom_concat(is_, Pref, Func),
    Body =.. [Func, Value],
    necki,
    Body.

inner_cast(Type, Value, C) :-
    cd_prefix(Type, Pref, EAL),
    append([Value|EAL], [C], AL),
    Body =.. [Pref|AL],
    necki,
    Body.

do_eval_cputime(T, V) :-
    X is cputime,
    inner_cast(T, X, V).

:- table
    do_eval_z/2,
    eepsilon/2.

do_eval_z(Type, C) :- cast(Type, 0, C).

eepsilon(T, E) :-
    reserve_eps(N),
    eval(T, N*epsilon, E).

eepsilon(T, N, E) :-
    eepsilon(T, R),
    eval(T, R*N, E).

compare(Type, Op, A, B) :-
    eval(Type, A, X),
    eval(Type, B, Y),
    compare_b(Op, Type, X, Y).

near_compare(Type, Op, A, B) :-
    eval(Type, A, X),
    eval(Type, B, Y),
    near_compare_b(Type, Op, X, Y).

near_compare_b(Type, Op, X, Y) :-
    ( compare_b(=, Type, X, Y)
    ->compare_eq(Op)
    ; eepsilon(Type, max(abs(X), abs(Y)), E),
      compare(Op, Type, X, Y, E)
    ).

compare(=,  T, A, B, E) :- compare(T, =<, abs(A - B), E).
compare(=<, T, A, B, E) :- compare(T, =<, A - B, E).
compare(>=, T, A, B, E) :- compare(T, =<, B - A, E).
compare(<,  T, A, B, E) :- compare(T, >, B - A, E).
compare(>,  T, A, B, E) :- compare(T, >, A - B, E).
compare(\=, T, A, B, E) :- compare(T, >, abs(A - B), E).

compare_b(Op, Type, X, Y) :-
    op_pred(Op, Pred),
    Body =.. [Pred, Type, X, Y],
    necki,
    Body.

Head :-
    op_pred(_, Pred),
    Head =.. [Pred, Type, X, Y],
    cd_prefix(Type, Pref, _),
    atomic_list_concat([Pref, '_', Pred], F),
    Body =.. [F, X, Y],
    necki,
    Body.

Head :-
    distinct(Pred, expr_pred(_, Pred)),
    Pred =.. [Name|AL],
    Head =.. [Name, Type, C|AL],
    cd_prefix(Type, Pref, EAL),
    atomic_list_concat([Pref, '_', Name], BN),
    append(EAL, [C|AL], BL),
    Body =.. [BN|BL],
    necki,
    Body.
