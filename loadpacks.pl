:- use_module(xlibrary/prolog/packloader).
:- use_module(library(thread)).
:- use_module(library(apply)).
% :- use_module(library(pldoc/doc_htmlsrc)).

:- meta_predicate loadpacks(1).

loadpacks(Loader) :-
    findall(Pack, package(Pack), PackL),
    scanpacks(
        PackL,
        pack_load_local(
            [exclude(
                 [clpcd/prolog/cd_common,
                  clpcd/prolog/eval,
                  idfpml/prolog/bid_auto,
                  mpfr/prolog/floatn_auto,
                  mpfr/prolog/floatn_reserve_eps,
                  xtools/prolog/ws_source,
                  refactor/prolog/i18n/i18n_trans,
                  refactor/prolog/ref_expand,
                  refactor/prolog/ref_replace,
                  refactor/prolog/ref_replacers,
                  xlibrary/prolog/compound_expand,
                  refactor/prolog/ref_scenarios,
                  stchecks/prolog/stchecks
                 ])
            ], Loader),
        pack_set_local_path).
