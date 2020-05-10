:- [plsloader].
:- use_module(library(thread)).
:- use_module(library(apply)).
% :- use_module(library(pldoc/doc_htmlsrc)).

:- meta_predicate loadpacks(1).

loadpacks(Loader) :-
    scanpacks(
        pack_load_local(
            [exclude(
                 [refactor/prolog/i18n/i18n_trans,
                  xtools/prolog/ws_source,
                  refactor/prolog/ref_expand,
                  refactor/prolog/ref_replace,
                  refactor/prolog/ref_replacers,
                  % library(compound_expand),
                  refactor/prolog/ref_scenarios,
                  xtools/prolog/checkers,
                  playground/prolog/float/floatn,
                  playground/prolog/float/floatn_so
                 ])
            ], Loader)).
