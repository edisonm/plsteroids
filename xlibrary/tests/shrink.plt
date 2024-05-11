:- begin_tests(shrink).

:- use_module(library(apply)).
:- use_module(library(shrink)).

gen_term_test(Term, Term) :-
    current_module(M),
    current_predicate(M:F/A),
    functor(H, F, A),
    F \= escape_atom,
    \+ predicate_property(M:H, imported_from(_)),
    catch(clause(M:H, B, _), _, fail),
    ( B = true
    ->Term = (M:H)
    ; Term = (M:(H:-B))
    ).

test(shrink1) :-
    forall(expand(shrink(gen_term_test(Term1)), Term2),
           assertion(Term1 == Term2)).

:- end_tests(shrink).
