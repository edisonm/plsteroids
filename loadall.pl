
:- [library(swi/plprops)].
:- [library(swi/assertions)].
:- [library(swi/nativeprops)].
:- [library(swi/termtyping)].
:- [library(swi/basicprops)].
:- [library(foreign/foreign_interface)].
:- [library(foreign/foreign_generator)].
:- [library(foreign/foreign_props)].
:- [library(assertions/examples/assrt_example)].
:- [library(swi/ctchecks)].
:- [library(swi/rtchecks)].
:- [library(swi/rtchecks_lib)].

:- expects_dialect(ciao).
:- [rtchecks/prolog/rtchecks/examples/rtchecks_example2].
:- use_module(rtchecks/prolog/rtchecks/examples/rtchecks_example, []).
:- use_module(rtchecks/prolog/rtchecks/examples/rtchecks_example3, []).
:- [rtchecks/prolog/rtchecks/examples/rtchecks_disc].
:- use_module(rtchecks/prolog/rtchecks/examples/rtchecks_inline, []).
:- [rtchecks/prolog/rtchecks/examples/rtc_external].
:- expects_dialect(swi).

:- [library(call_in_module_file)].
:- [library(clambda)].
:- [library(context_values)].
:- [library(dict_utils)].
:- [library(pe)].
:- [library(termdiff)].
:- [library(audit/domains/abstract_domain_fail)].
:- [library(audit/domains/abstract_domain_product)].
:- [library(audit/domains/abstract_domain_sideff)].
% :- [library(audit/check_abstract_domains)].
:- [library(i18n/i18n_refactor)].
:- [library(i18n/i18n_expansion)].
:- [library(list_intervals)].
:- [library(mapeach)].
:- [library(ntabling)].
:- [library(called_from)].
:- [library(comment_data)].
:- [library(defer)].
:- [library(nitrace)].
:- [library(tabulator)].
:- [library(ws_source)].

% :- [xtools/prolog/audit/check_auto].

/*
:- [xlibrary/prolog/normalize_head].
:- [xlibrary/prolog/implementation_module].
:- [xlibrary/prolog/remove_dups].
:- [xlibrary/prolog/compact_pi_list].
:- [xlibrary/prolog/i18n/i18n_op].
:- [xlibrary/prolog/i18n/i18n_parser].
:- [xlibrary/prolog/i18n/i18n_support].
:- [xlibrary/prolog/i18n/i18n_expansion].
:- [xlibrary/prolog/ungroup_keys_values].
:- [xlibrary/prolog/list_sequence].
:- [xlibrary/prolog/change_alias].
:- [xlibrary/prolog/pe].
:- [xlibrary/prolog/ntabling].
:- [xlibrary/prolog/group_pairs_or_sort].
:- [xlibrary/prolog/mklinear].
:- [xlibrary/prolog/infer_alias].
:- [xlibrary/prolog/maplist_dcg].
:- [xlibrary/prolog/list_intervals].
:- [xlibrary/prolog/sequence_list].
:- [xlibrary/prolog/compound_expand].
:- [xlibrary/prolog/mapeach].
:- [xlibrary/prolog/normalize_pi].
:- [xtools/prolog/tabulator].
:- [xtools/prolog/defer].
:- [xtools/prolog/ws_cover].
:- [xtools/prolog/database_fact].
:- [xtools/prolog/called_from].
:- [xtools/prolog/static_strip_module].
:- [xtools/prolog/ontrace].
:- [xtools/prolog/location_utils].
:- [xtools/prolog/implemented_in].
:- [xtools/prolog/qualify_meta_goal].
:- [xtools/prolog/nitrace].
:- [xtools/prolog/record_locations].
:- [xtools/prolog/current_defined_predicate].
:- [xtools/prolog/included_files].
:- [xtools/prolog/audit/check_trivial_fails].
:- [xtools/prolog/audit/abstract_domain_decls].
:- [xtools/prolog/audit/abstract_domain].
:- [xtools/prolog/audit/check_unused].
:- [xtools/prolog/audit/check_meta_decls].
:- [xtools/prolog/audit/check_non_mutually_exclusive].
:- [xtools/prolog/audit/check_undefined].
:- [xtools/prolog/audit/check_non_loaded].
:- [xtools/prolog/audit/check_abstract_domains].
:- [xtools/prolog/audit/check_wrong_dynamic].
:- [xtools/prolog/audit/check_deprecated].
:- [xtools/prolog/audit/check_dupcode].
:- [xtools/prolog/audit/audit].
:- [xtools/prolog/auditable_predicate].
:- [xtools/prolog/referenced_by].
:- [xtools/prolog/audits].
:- [xtools/prolog/comment_data].
:- [xtools/prolog/gcover].
:- [xtools/prolog/is_entry_point].
:- [xtools/prolog/ws_browser].
:- [xtools/prolog/option_utils].
:- [xtools/prolog/ws_source].
:- [xtools/prolog/scc].
:- [xtools/prolog/module_files].
*/

checkall(_, [dir('.')]).
