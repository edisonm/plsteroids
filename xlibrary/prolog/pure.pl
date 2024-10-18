:- module(pure,
          [ is_pure_pred/1,
            is_pure_pred/2,
            is_pure_clause/1,
            is_pure_body/1,
            is_pure_body/2
          ]).

:- use_module(library(lists)).
:- use_module(library(resolve_calln)).
:- use_module(library(qualify_meta_goal)).

:- meta_predicate
        is_pure_pred(0 ),
        is_pure_pred(0, 1 ),
        is_pure_body(0 ),
        is_pure_body(0, 1 ).

fail_1(_) :- fail.

is_pure_pred(Head) :-
    is_pure_pred(Head, fail_1, []).

%!  is_pure_pred(:Pred, :Impure) is semidet
%
%   Succeeds if the predicate is pure-prolog, that is, it does not have
%   side-effects and call to Impure fails for all dependent predicates.  We
%   check that by ensuring that the predicate is implemented only using prolog
%   clauses or other pure-prolog predicates.  Fails otherwise or if it is unable
%   to determine it.

is_pure_pred(Head, Impure) :-
    is_pure_pred(Head, Impure, []).

%!  is_pure_clause(Ref) is semidet
%
%   Succeeds if the clause referenced by Ref has been implemented using only
%   pure-prolog predicates.

is_pure_clause(Ref) :-
    is_pure_clause(Ref, fail_1, []).

is_pure_pred(M:Head, Impure, Stack1) :-
    \+ call(Impure, M:Head),
    predicate_property(M:Head, interpreted),
    findall(Ref, ( catch(clause(M:Head, _, Ref), _, fail),
                   \+ memberchk(Ref, Stack1)
                 ), RefL),
    append(RefL, Stack1, Stack),
    forall(member(Ref, RefL),
           is_pure_clause(Ref, Impure, Stack)).

is_pure_clause(Ref, Impure, Stack) :-
    clause_property(Ref, module(CM)),
    catch(clause(M:Head, Body, Ref), _, fail),
    % is not defined in init.pl:
    M:Head \== CM:Body,
    M:call(Head) \== CM:Body,
    is_pure_body(Body, CM, Impure, Stack).

% Note: ->/2, \+/1, *->/2, !, are not consider pure prolog, you can prove that by
% implementing var/1 using those, since var/1 is not pure prolog. For instance:
%
% var(A) :- \+ (A = a), \+ (A = b).

% In general, any predicate that check the instantiation status of a variable is
% not pure-prolog

is_pure_body(M:G) :-
    is_pure_body(G, M, fail_1, []).

is_pure_body(M:G, Impure) :-
    is_pure_body(G, M, Impure, []).

is_pure_body(G, M, Impure, _) :-
    var(G),
    !,
    freeze(G, is_pure_pred(M:G, Impure)).
is_pure_body(true, _, _, _) :- !.
is_pure_body(fail, _, _, _) :- !.
is_pure_body(_=_,  _, _, _) :- !.
is_pure_body(\+ _, _, _, _) :- !, fail.
is_pure_body(@(G, CM), _, Impure, Stack) :-
    !,
    strip_module(CM:G, M, H),
    is_pure_body_mod(H, M, CM, Impure, Stack).
is_pure_body(M:G, _, Impure, Stack) :-
    !,
    is_pure_body_mod(G, M, M, Impure, Stack).
is_pure_body((A,B), M, Impure, Stack) :-
    !,
    is_pure_body(A, M, Impure, Stack),
    is_pure_body(B, M, Impure, Stack).
is_pure_body((A;B), M, Impure, Stack) :-
    !,
    is_pure_body(A, M, Impure, Stack),
    is_pure_body(B, M, Impure, Stack).
is_pure_body(CallN, M, Impure, Stack) :-
    do_resolve_calln(CallN, Call),
    is_pure_body(Call, M, Impure, Stack).
is_pure_body(phrase(DCG, L, T), M, Impure, Stack) :-
    !,
    dcg_translate_rule(('$head$' --> DCG, '$sink$'), _, ('$head$'(L, _) :- Lits, '$sink$'(T, _)), _),
    is_pure_body(Lits, M, Impure, Stack).
is_pure_body(phrase(DCG, L), M, Impure, Stack) :-
    !,
    is_pure_body(phrase(DCG, L, []), M, Impure, Stack).
is_pure_body(H, M, Impure, Stack) :-
    ( predicate_property(M:H, meta_predicate(Meta))
    ->qualify_meta_goal(M:H, Meta, Goal)
    ; Goal = H
    ),
    is_pure_pred(M:Goal, Impure, Stack).

/*
is_pure_body_mod(G, M, CM, Stack) :-
    ( atom(M),
      atom(CM)
    ->setup_call_cleanup(
          ( '$current_source_module'(OldM),
            '$set_source_module'(CM)
          ),
          is_pure_body(G, M, Stack),
          '$set_source_module'(OldM))
    ; true
    ).
*/

is_pure_body_mod(H, M, _, Impure, Stack) :-
    is_pure_body(H, M, Impure, Stack).
