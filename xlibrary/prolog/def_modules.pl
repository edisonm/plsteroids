:- module(def_modules,
          [ '$def_modules'/2
          ]).

:- module_transparent '$def_modules'/2.

'$def_modules'(Preds, MList) :-
    '$current_source_module'(Source),
    strip_module(Source:Preds, M, PL),
    ( '$defined_predicate'(M:'$module_expansors'(_, _)),
      M:'$module_expansors'(PL, MList)
    ->true
    ; system:'$def_modules'(M:PL, MList)
    ).
