:- begin_tests(rtchecks).

:- multifile
    user:message_property/2.

:- dynamic
    user:error_on_co/0.

:- use_module(library(call_in_dir)).
:- use_module(library(substitute)).
:- use_module(library(rtchecks_utils)).
:- use_module(library(intercept)).
:- use_module(library(libprops)).

user:message_property(_, location_prefix(_, '', 'ERROR: ')).
user:message_property(_, stream(current_output)) :- user:error_on_co.

:- set_prolog_flag(runtime_checks, yes).
:- set_prolog_flag(rtchecks_check, yes).

test(rtc_external) :-
    call_in_module_dir(plunit_rtchecks,
                       ( ['../examples/rtc_external'],
                         save_rtchecks(with_rtchecks(test_ex)),
                         load_rtchecks(E),
                         % Unload it to avoid further problems with format/3:
                         unload_file('../examples/rtc_external')
                       )),
    assertion(E = [assrchk(error(comp,
                                 _:functor1(0, 0, 0 ),
                                 [_/fails-[not_fails]],
                                 _,
                                 _)),
                   assrchk(error(success,
                                 _:functor1(0, 0, 0 ),
                                 [_/instan(rtc_external:atom(0))-[]],
                                  _,
                                 _))]).

test(rtcompile) :-
    %set_prolog_flag(check_assertions, [defined, is_prop, ctcheck]),
    call_in_module_dir(plunit_rtchecks,
                       ( use_module('../examples/rtchecks_disc', []),
                         ['../examples/rtchecks_example2'],
                         use_module('../examples/rtchecks_example', [])
                       )),
    %set_prolog_flag(check_assertions, []).
    retractall(user:error_on_co).

:- ['../examples/rtchecks_example3'].

test(rtexec1) :-
    save_rtchecks(with_rtchecks(test1)),
    load_rtchecks(E),
    assertion(E=[assrchk(error(pp_check, check/1,
                               [_/(rtchecks_example3: (0>0 ))-[]], _, _))]).

test(rtexec2) :-
    intercept(with_rtchecks(test1), Error, print_message(information, Error)).

test(rtexec3) :-
    ignore(save_rtchecks(with_rtchecks(p(_)))),
    % ignore(call_rtc(with_rtchecks(p(_)))),
    load_rtchecks(E),
    assertion(E=[assrchk(error(comp, _:qq, [file(_, _, _, _)/not_fails-[failure]],
                               clause_pc(_, _), file(_, _, _, _))),
                 assrchk(error(comp, _:r, [file(_, _, _, _)/det-[fails]],
                               clause_pc(_, _), file(_, _, _, _)))]).

% The next  two tests  implements run-time checking  via instrumentation  of the
% predicate  being run-time  checked.  Apart  of  that, be  careful, since  they
% contain  several combinations  of assertions,  so don't  modifiy the  run-time
% checks without being sure that they are Ok

test(rtexec4) :-
    save_rtchecks(fullasr(3,_B)),
    load_rtchecks(E),
    assertion(E=[assrchk(error(success,_:fullasr(3,3),
                               [_/instan(rtchecks_example3:family(3))-[]], _, _))]).

test(rtexec5) :-
    save_rtchecks(fullasr(a,_B)),
    load_rtchecks(E),
    assertion(E=[]).

:- ['../examples/rtpred1'].

test(rtpred1) :-
    save_rtchecks(p1(1)),
    load_rtchecks(E),
    assertion(E=[_]).

:- end_tests(rtchecks).
