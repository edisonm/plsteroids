:- expects_dialect(ciao).
:- use_package(library(rtchecks)).
:- expects_dialect(swi).

:- set_prolog_flag(autoload,false).

:- [library(swi/plprops)].
:- [library(swi/assertions)].
:- [library(swi/nativeprops)].
:- [library(swi/termtyping)].
:- [library(swi/basicprops)].
:- [library(foreign/foreign_interface)].
:- [library(foreign/foreign_generator)].
:- [library(foreign/foreign_props)].
:- [library(assertions/examples/assrt_example)].
:- [library(assrt_interface)].
:- [library(swi/rtchecks)].
:- [library(swi/rtchecks_lib)].
:- [library(swi/libprops)].
:- [library(call_in_module_file)].
:- [library(file_includes)].
:- [library(clambda)].
:- [library(tabling)].
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

:- [library(audit/check_abstract_domains)].
:- [library(audit/domains/abstract_domain_fail)].
:- [library(audit/domains/abstract_domain_product)].
:- [library(audit/domains/abstract_domain_sideff)].

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
