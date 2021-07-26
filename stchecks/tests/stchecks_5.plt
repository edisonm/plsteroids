:- begin_tests(stchecks_5).

:- use_module(library(infer_meta)).
:- use_module(library(checker)).
:- use_module(library(check_wrong_dynamic)).
:- use_module(stchecks_hooks).

:- use_module(cwdc).

test(cwd_5, [setup(cleanup_inferred_meta)]) :-
    check_results(wrong_dynamic, Results, [files([stchecks/tests/cwdc])]),
    assertion(Results = []).

:- end_tests(stchecks_5).
