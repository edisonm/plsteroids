:- [plsdirs].
:- [plsloader].
:- use_module(library(pldoc/doc_htmlsrc)).
:- set_prolog_flag(autoload, false).
:- packages(Packs),
   scanpacks(Packs,
             pack_load_files(
                 [exclude(
                      [library(i18n/i18n_trans),
                       library(ws_source),
                       library(ref_expand),
                       library(ref_replace),
                       library(ref_replacers),
                       library(compound_expand),
                       library(ref_scenarios),
                       library(checkers),
                       checkers(check_abstract_domains)
                      ])])).
:- set_prolog_flag(autoload,true).
