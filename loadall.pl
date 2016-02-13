
:- set_prolog_flag(autoload, false).

:- use_module(library(apply_macros)). % Load it proactively
:- [assertions(assertions)].
:- [assertions(plprops)].
:- [assertions(nativeprops)].
:- [assertions(termtyping)].
:- [assertions(basicprops)].
:- [assertions(foreign/foreign_interface)].
:- [assertions(foreign/foreign_generator)].
:- [assertions(foreign/foreign_props)].
:- [assertions(libprops)].
:- [xlibrary(compact_list)].
:- [xtools(assrt_meta)].
:- [assertions(assrt_interface)].
:- [rtchecks(rtchecks)].
:- [xlibrary(call_in_dir)].
:- [xtools(file_includes)].
:- [xlibrary(clambda)].
:- [xlibrary(tabling)].
:- [xlibrary(abstract_slicer)].
:- [xlibrary(atomics_atom)].
:- [xlibrary(atomics_string)].
:- [xtools(called_by_body)].
:- [xtools(scc)].
:- [refactor(mapargs)].
:- [xlibrary(context_values)].
:- [xlibrary(dict_utils)].
:- [xlibrary(pe)].
:- [xlibrary(near)].
:- [xlibrary(termdiff)].
:- [xlibrary(trim_utils)].
:- [xtools(included_files)].

:- [checkers(check_abstract_domains)].
:- [domains(abstract_domain_fail)].
:- [domains(abstract_domain_product)].
:- [domains(abstract_domain_sideff)].

:- [refactor(refactor)].
:- [xlibrary(i18n/i18n_op)].
:- [refactor(i18n/i18n_refactor)].
:- [xlibrary(i18n/i18n_expansion)].
:- [xlibrary(list_intervals)].

:- [xlibrary(mapeach)].
:- [xlibrary(ntabling)].
:- [xtools(called_from)].
:- [xtools(implemented_in)].
:- [xtools(comment_data)].
:- [xtools(defer)].
:- [xtools(nitrace)].
:- [xtools(tabulator)].
:- [refactor(file_to_module)].

:- set_prolog_flag(autoload,true).

:- [xtools(ws_source)].
