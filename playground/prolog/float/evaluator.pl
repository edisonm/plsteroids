:- module(evaluator, [evalexpr/3]).

:- use_module(library(extend_args)).
:- use_module(library(foldargs)).
:- use_module(library(mapargs)).

:- multifile
        evalfunc/3,
        castexpr/3,
        domain_value/3,
        domain_order/2,
        domain_args/2.

%% evalexpr(Domain, +Expr, -Value) is semidet.

evalexpr(_, Var, _) :-
    var(Var),
    !,
    throw(error(instantiation_error, context(evalexpr:evalexpr/3, _))).
evalexpr(Domain, Elem, Value) :-
    domain_value(Domain, Elem, Value),
    !.
evalexpr(Domain, Expr1, Value) :-
    castexpr(Expr1, Domain1, Expr),
    !,
    evalexpr(Domain1, Expr, Value1),
    domain_value(Domain, Value1, Value).
evalexpr(Domain, Expr, Value) :-
    functor(Expr, F, A),
    functor(EVal, F, A),
    functor(DArg, F, A),
    ( nonvar(Domain)
    ->EDom = DArg,
      domain_args(Domain, DArg),
      mapargs(join_domain(Domain), DArg),
      mapargs(evalexpr, EDom, Expr, EVal),
      evalfunc(Domain, EVal, Value)
    ; functor(EDom, F, A),
      mapargs(evalexpr, EDom, Expr, EVal),
      ignore(order_by([desc(Order)],
                      ( arg(_, EDom, Domain1),
                        nonvar(Domain1),
                        domain_order(Domain1, Order)
                      ))),
      Domain = Domain1,
      freeze(Domain,
             ( domain_args(Domain, DArg),
               mapargs(join_domain(Domain), DArg),
               mapargs(cast_domain, EDom, DArg, EVal, EC),
               evalfunc(Domain, EC, Value)
             ))
    ).

join_domain(Domain, Domain1) :- ignore(Domain=Domain1).

%% cast_domain(Domain1, Domain, Value1, Value)
%
%   Converts from Value1 of Domain1 to Value of Domain

cast_domain(Domain1, Domain, Value1, Value) :-
    ( Domain = Domain1
    ->Value = Value1
    ; domain_value(Domain, Value1, Value)
    ).
