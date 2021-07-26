:- begin_tests(stchecks_unused).

:- use_module(library(infer_meta)).
:- use_module(library(checker)).
:- use_module(library(check_unused)).
:- use_module(stchecks_hooks).

:- use_module(cwda).

% Warning: * cwda.pl:5: unreferenced dynamic(def,cwda,system:assertz(b(1))): cwdb:b/1
% Warning: * cwdb.pl:1: unreferenced predicate([exported,dynamic]): cwdb:b/1
% Warning: * cwdb.pl:9: unreferenced clause(2): cwdb:q/1-2
% Warning: * cwdb.pl:11: unreferenced predicate([exported,static]): cwdb:r/1
% Warning: * cwda.pl:1:17: unreferenced export: cwda:a/0-exp
% Warning: * cwdb.pl:1:17: unreferenced export: cwdb:b/1
% Warning: * cwdb.pl:1:27: unreferenced export: cwdb:q/1-exp
% Warning: * cwdb.pl:1:32: unreferenced export: cwdb:r/1
% Warning: * cwdb.pl:3:11: unreferenced dynamic: cwdb:b/1

test(cu) :-
    % showcheck(unused, [files([stchecks/tests/cwda, stchecks/tests/cwdb])]),
    check_results(unused, Results, [files([stchecks/tests/cwda, stchecks/tests/cwdb])]),
    assertion(length(Results, 9)).

:- end_tests(stchecks_unused).
