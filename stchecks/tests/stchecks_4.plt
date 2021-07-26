:- begin_tests(stchecks_4).

:- use_module(library(infer_meta)).
:- use_module(library(checker)).
:- use_module(library(check_wrong_dynamic)).
:- use_module(stchecks_hooks).

:- use_module(cwda).
:- use_module(cwdb).

test(cwd_4) :- % There is an issue that does not refers to module cwda
    check_results(wrong_dynamic, Results, [module(cwda), files([stchecks/tests/cwdb])]),
    assertion(Results = []).

:- end_tests(stchecks_4).
