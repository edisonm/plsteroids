:- module(def_modules,
          [ '$def_modules'/2,
            type_expansors/2
          ]).

type_expansors(term, [term_expansion/4, term_expansion/2]).
type_expansors(goal, [goal_expansion/4, goal_expansion/2]).

:- module_transparent '$def_modules'/2.

'$def_modules'(Preds, MList) :-
    system:'$def_modules'(Preds, MList1),
    ( '$current_source_module'(Source),
      strip_module(Source:Preds, M, PL),
      '$defined_predicate'(M:'$module_expansors'(_, _)),
      type_expansors(Type, PL),
      M:'$module_expansors'(Type, MList2)
    ->'$append'(MList2, MList1, MList)
    ; MList = MList1
    ).
