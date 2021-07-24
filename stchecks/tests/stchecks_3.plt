:- begin_tests(stchecks_3).

:- use_module(library(infer_meta)).
:- use_module(library(checker)).
:- use_module(library(check_wrong_dynamic)).
:- use_module(stchecks_hooks).

:- use_module(cwda).

test(cwd_3) :-
    check_results(wrong_dynamic, Results, [files([xtools/tests/cwda])]),
    assertion(Results = []).

:- end_tests(stchecks_3).
