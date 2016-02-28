
:- set_prolog_flag(autoload, false).

:- use_module(library(apply_macros)). % Load it proactively
:- [library(record_locations)].
:- [library(assertions)].
:- [library(plprops)].
:- [library(nativeprops)].
:- [library(termtyping)].
:- [library(basicprops)].
:- [library(foreign/foreign_interface)].
:- [library(foreign/foreign_generator)].
:- [library(foreign/foreign_props)].
:- [library(libprops)].
:- [library(compact_list)].
:- [library(assrt_meta)].
:- [library(argument_chains)].
:- [library(assrt_interface)].
:- [library(rtchecks)].
:- [library(call_in_dir)].
:- [library(file_includes)].
:- [library(clambda)].
:- [library(tabling)].
:- [library(abstract_slicer)].
:- [library(atomics_atom)].
:- [library(atomics_string)].
:- [library(called_by_body)].
:- [library(scc)].
:- [library(mapargs)].
:- [library(context_values)].
:- [library(dict_utils)].
:- [library(pe)].
:- [library(near)].
:- [library(termdiff)].
:- [library(trim_utils)].
:- [library(included_files)].

:- [checkers(check_abstract_domains)].
:- [domains(abstract_domain_fail)].
:- [domains(abstract_domain_product)].
:- [domains(abstract_domain_sideff)].

:- [library(refactor)].
:- [library(i18n/i18n_op)].
:- [library(i18n/i18n_refactor)].
:- [library(i18n/i18n_expansion)].
:- [library(list_intervals)].

:- [library(mapeach)].
:- [library(ntabling)].
:- [library(called_from)].
:- [library(implemented_in)].
:- [library(comment_data)].
:- [library(defer)].
:- [library(nitrace)].
:- [library(tabulator)].
:- [library(file_to_module)].

:- set_prolog_flag(autoload,true).

:- [library(ws_source)].
