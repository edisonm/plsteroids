:- [plsloader].
%:- use_module(library(pldoc/doc_htmlsrc)).

:- meta_predicate loadpacks(1).

loadpacks(Loader) :-
    packages(Packs),
    scanpacks(Packs,
              pack_load_files(
                  [exclude(
                       [library(i18n/i18n_trans),
                        library(ws_source),
                        library(ref_expand),
                        library(ref_replace),
                        library(ref_replacers),
                        % library(compound_expand),
                        library(ref_scenarios),
                        library(checkers),
                        library(float/floatn),
                        library(float/floatn_so),
                        library(andorra/andorra_builtins_exports)
                       ])], Loader)).
