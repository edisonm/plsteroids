:- module(foreseen,
          [ foreseen/1
          ]).

:- meta_predicate foreseen(0 ).

%!  foreseen(:Goal)
%
%   like call/1, but precalculates the next match to prevent green points

foreseen(Goal) :-
    copy_term(Goal, Term),
    PrevSol = s([]),
    ( call(Term),
      ( PrevSol = s(Goal)
      ->nb_setarg(1, PrevSol, Term)
      ; nb_setarg(1, PrevSol, Term),
        fail
      )
    ; PrevSol = s(Goal)
    ).
