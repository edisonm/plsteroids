:- module(def_modules,
          [ '$def_modules'/2
          ]).

:- module_transparent '$def_modules'/2.

% Warning: note that we still use system:'$def_modules'/2 inside this, since its
% output could change during the load of the module

'$def_modules'(Preds, MList) :-
    '$current_source_module'(Source),
    strip_module(Source:Preds, M, PL),
    system:'$def_modules'(M:PL, MTail),
    ( '$defined_predicate'(M:'$module_expansors'(_, _, _)),
      M:'$module_expansors'(PL, MList, MTail)
    ->true
    ; MList = MTail
    ).
