:- begin_tests(stchecks_2).

:- use_module(library(infer_meta)).
:- use_module(library(checker)).
:- use_module(library(check_wrong_dynamic)).
:- use_module(stchecks_hooks).

:- use_module(cwda).
:- use_module(cwdb).

test(cwd_2) :-
    check_results(wrong_dynamic, Results, [files([stchecks/tests/cwda, stchecks/tests/cwdb])]),
    assertion(Results = []).

:- end_tests(stchecks_2).
