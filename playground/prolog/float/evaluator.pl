:- module(evaluator, [evalexpr/3]).

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
    functor(EDAr, F, A),
    ( nonvar(Domain)
    ->EDom = EDAr,
      domain_args(Domain, EDAr),
      mapargs(join_domain(Domain), EDAr),
      mapargs(evalexpr, EDom, Expr, EVal),
      evalfunc(Domain, EVal, Value)
    ; functor(EDom, F, A),
      mapargs(evalexpr, EDom, Expr, EVal),
      ( order_by([asc(Order)],
                 ( domain_args(Domain1, EDAr),
                   mapargs(compare_order, EDom, EDAr),
                   domain_order(Domain1, Order)
                 ))
      ->Domain = Domain1
      ; freeze(Domain, domain_args(Domain, EDAr))
      ),
      freeze(Domain,
             ( mapargs(join_domain(Domain), EDAr),
               mapargs(cast_domain, EDom, EDAr, EVal, EC),
               evalfunc(Domain, EC, Value)
             ))
    ).

compare_order(DomainE, DomainA) :-
    domain_order(DomainE, OrderE),
    domain_order(DomainA, OrderA),
    OrderE@=<OrderA.

join_domain(Domain, Domain1) :- ignore(Domain=Domain1).

%%  cast_domain(Domain1, Domain, Value1, Value)
%
%   Converts from Value1 of Domain1 to Value of Domain

cast_domain(Domain1, Domain, Value1, Value) :-
    ( Domain = Domain1
    ->Value = Value1
    ; domain_value(Domain, Value1, Value)
    ).
