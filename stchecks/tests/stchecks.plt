:- begin_tests(stchecks).

:- multifile
    user:message_property/2.

:- dynamic
    user:error_on_co/0.

user:message_property(_, stream(current_output)) :- user:error_on_co.

:- use_module(library(filesex)).
:- use_module(library(record_locations)).
:- use_module(library(comment_data)).
:- use_module(library(call_in_dir)).
:- use_module(library(checker)).
:- use_module(library(check_assertions)).
:- use_module(library(listing), []).
:- use_module(library(clambda)).
:- init_expansors.

:- set_setting(listing:tab_distance, 0). % Use only spaces, no tabs

:- comment_data:enable.

test_ct(CommentName, Options) :-
    setup_call_cleanup(
        ( set_prolog_flag(verbose, silent),
          assert(user:error_on_co)
        ),
        do_test_ct(CommentName, Options),
        ( set_prolog_flag(verbose, normal),
          % set_prolog_flag(check_assertions, []).
          retractall(user:error_on_co)
        )).

do_test_ct(CommentName, Options) :-
    notrace(with_output_to(string(Result), showcheck(assertions, Options))),
    comment_data(CommentName, Pattern),
    module_property(ctcex, file(File)),
    directory_file_path(Dir, _, File),
    directory_file_path(Dir, '', AD),
    atom_string(AD, SD),
    replace_noisy_strings(SD, Result, AResult),
    ( Pattern \== AResult
    ->format("~n~s", [AResult])
    ; true
    ),
    assertion(Pattern == AResult).

:- use_module(ctcex).

/* $ctcex$
Warning: Check asssertions
Warning: -----------------
Warning: The predicates contain assertions that are inconsistent
Warning: with the implementation.
Warning: 
ctcex.pl:xx: In the body of ctcex:q/0:
Assertion failure for ctcex:a(1,b).
ctcex.pl:xx:     In *calls*, unsatisfied properties: 
        ctcex.pl:xx: compat(ctcex:list(b)).

ctcex.pl:16: In the head of ctcex:a/2:
Assertion failure for ctcex:a(a,b).
ctcex.pl:xx:     In *calls*, unsatisfied properties: 
        ctcex.pl:xx: compat(ctcex:int(a)).
        ctcex.pl:xx: compat(ctcex:list(b)).

ctcex.pl:xx: In assertions of [ctcex:b/2]:
    ctcex.pl:xx: ctcex:is_3/1 is not a property
ctcex.pl:xx: In assertions of [ctcex:b/2]:
    ctcex.pl:xx: ctcex:is_2/1 is not a property
ctcex.pl:xx: In assertions of [ctcex:b/2]:
    ctcex.pl:xx: In call to ctcex:is_num/2:
Assertion failure for ctcex:is_num(a,A).
ctcex.pl:xx:     In *calls*, unsatisfied properties: 
        ctcex.pl:xx: compat(ctcex:int(a)).

    ctcex.pl:xx: In call to ctcex:is_num/2:
Assertion failure for ctcex:is_num(b,A).
ctcex.pl:xx:     In *calls*, unsatisfied properties: 
        ctcex.pl:xx: compat(ctcex:int(b)).

*/

test(ctcex) :-
    test_ct(ctcex, [module(ctcex), method(source)]).

:- use_module(p1).

/* $p11$
Warning: Check asssertions
Warning: -----------------
Warning: The predicates contain assertions that are inconsistent
Warning: with the implementation.
Warning: 
p1.pl:17:4: In the body of p1:p0/0:
Assertion failure for p1:p1(p1:q2).
p1.pl:10:8:     In *calls*, unsatisfied properties: 
        p1.pl:10:11: compat(typeprops:goal(0,p1:q2)).

p1.pl:23:4: In the body of p1:p2/1:
Assertion failure for p1:q(A,B,C).
p1.pl:20:8:     In *calls*, unsatisfied properties: 
        p1.pl:20:11: instan(p1:atm(A)).

Assertion failure for p1:q(A,B,C).
p1.pl:19:8:     In *calls*, unsatisfied properties: 
        p1.pl:19:11: instan(p1:int(A)).

p1.pl:27:4: In the body of p1:p2/1:
Assertion failure for p1:q(X,Y,Z).
p1.pl:20:8:     In *calls*, unsatisfied properties: 
        p1.pl:20:11: instan(p1:atm(X)).

Assertion failure for p1:q(X,Y,Z).
p1.pl:19:8:     In *calls*, unsatisfied properties: 
        p1.pl:19:11: instan(p1:int(X)).

*/
test(ctmeta1) :-
    test_ct(p11, [module(p1), method(source)]).

/* $p12$
Warning: Check asssertions
Warning: -----------------
Warning: The predicates contain assertions that are inconsistent
Warning: with the implementation.
Warning: 
p1.pl:15: In the body of p1:p0/0:
Assertion failure for p1:p1(p1:q2).
p1.pl:10:8:     In *calls*, unsatisfied properties: 
        p1.pl:10:11: compat(typeprops:goal(0,p1:q2)).

p1.pl:22: In the body of p1:p2/1:
Assertion failure for p1:q(A,B,C).
p1.pl:20:8:     In *calls*, unsatisfied properties: 
        p1.pl:20:11: instan(p1:atm(A)).

Assertion failure for p1:q(A,B,C).
p1.pl:19:8:     In *calls*, unsatisfied properties: 
        p1.pl:19:11: instan(p1:int(A)).

*/

test(ctmeta2) :-
    test_ct(p12, [module(p1), method(clause)]).

:- use_module(p2).

test(samename) :-
    set_prolog_flag(verbose, silent),
    assert(user:error_on_co),
    with_output_to(string(Result), showcheck(assertions, [module(p2)])),
    set_prolog_flag(verbose, normal),
    assertion(Result == ""),
    retractall(user:error_on_co).

replace_substrings_lc(L, C) -->
    { atomics_to_string(["ctcex.pl:", L, ":", C, ":"], SubS),
      Repl = "ctcex.pl:xx:"
    },
    replace_substrings(SubS, Repl).

replace_noisy_strings(SD) -->
    replace_substrings(SD, ""),
    replace_substrings("ERROR: ", ""),
    {findall(L, between(1, 50, L), Ls)},
    {findall(C, between(0, 30, C), Cs)},
    foldl([Cs] +\ L^foldl(replace_substrings_lc(L), Cs), Ls).

replace_substrings(SubS, Repl, String, Result) :-
    ( sub_string(String, Before, _, After, SubS)
    ->sub_string(String, 0, Before, _, BeforeS),
      sub_string(String, _, After, 0, AfterS),
      replace_substrings(SubS, Repl, AfterS, Tail),
      string_concat(BeforeS, Repl, ResultHead),
      string_concat(ResultHead, Tail, Result)
    ; Result = String
    ).

:- comment_data:disable.

:- end_tests(stchecks).
