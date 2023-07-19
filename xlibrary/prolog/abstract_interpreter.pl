/*  Part of Extended Libraries for Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(abstract_interpreter,
          [ abstract_interpreter/3,
            abstract_interpreter/4,
            abstract_interpreter_body/5,
            extra_clauses/4,              % +Goal, +Module, -Body, -From
            get_state/3,
            put_state/3,
            match_head/4,
            match_head_body/3,
            bottom/2,
            call_ai/1,
            eval_ai/1,
            skip_ai/1,
            intr_ai/1,
            match_noloops/4
          ]).

:- use_module(library(lists)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(resolve_calln)).
:- use_module(library(term_size)).
:- use_module(library(terms_share)).
:- use_module(library(neck)).
:- use_module(library(interface), []).

:- init_expansors.

/** <module> An abstract interpreter

This module provides  support to implement some  abstract interpreter-based code
scanner.  It tests statically an  oversimplification of the possible unification
of terms  and call  hooks over  head and  literals in  the body  of a  clause to
collect certain information.

@author Edison Mera
*/

:- meta_predicate
    abstract_interpreter(0,4,?),
    abstract_interpreter(0,4,+,-),
    abstract_interpreter_body(+,+,4,?,?),
    extra_clauses(+,+,-,-),
    match_head(0,*,*,*),
    match_head_body(0,*,*),
    match_noloops(0,*,*,*),
    call_ai(0),
    eval_ai(0),
    skip_ai(0).

:- multifile
    replace_goal_hook/3,
    replace_body_hook/3,
    evaluable_goal_hook/2,
    evaluable_body_hook/3.

:- dynamic
    evaluable_goal_hook/2.

:- discontiguous
    abstract_interpreter_body/5.

evaluable_body_hook(absolute_file_name(A, _, O), _, (ground(A), ground(O))).
evaluable_body_hook(atom_concat(A, B, C), _,
                    ( nonvar(A), nonvar(B)
                    ; nonvar(A), nonvar(C)
                    ; nonvar(B), nonvar(C)
                    )).
evaluable_body_hook(atomic_list_concat(A, _), _, ground(A)).
evaluable_body_hook(atomic_list_concat(A, B, C), _,
                    ( ground(A), ground(B)
                    ; ground(B), ground(C)
                    )).
evaluable_body_hook(thread_self(_), _, true).
evaluable_body_hook(atom_length(A, _), _, ground(A)).
evaluable_body_hook(upcase_atom(A, _), _, ground(A)).
evaluable_body_hook(downcase_atom(A, _), _, ground(A)).
evaluable_body_hook(string_lower(A, _), _, ground(A)).
evaluable_body_hook(string_upper(A, _), _, ground(A)).
evaluable_body_hook(nb_current(A, _), _, ground(A)).
evaluable_body_hook(_ is A, _, ground(A)).
evaluable_body_hook(A > B, _, (ground(A),ground(B))).
evaluable_body_hook(A >= B, _, (ground(A),ground(B))).
evaluable_body_hook(A < B, _, (ground(A),ground(B))).
evaluable_body_hook(A =< B, _, (ground(A),ground(B))).
evaluable_body_hook(A =:= B, _, (ground(A),ground(B))).
evaluable_body_hook(atom_codes(A, B), _, (ground(A);ground(B))).
evaluable_body_hook(atom_chars(A, B), _, (ground(A);ground(B))).
evaluable_body_hook(member(_, L), _, is_list(L)).
evaluable_body_hook(current_predicate(P), _, ground(P)).
evaluable_body_hook(current_module(M), _, ground(M)).
evaluable_body_hook(select(_, L, _), _, is_list(L)).
evaluable_body_hook(option(O, L), _, (is_list(L), nonvar(O))).
evaluable_body_hook(nth0(I, L, _), _, (is_list(L);nonvar(I))).
evaluable_body_hook(nth1(I, L, _), _, (is_list(L);nonvar(I))).
evaluable_body_hook(arg(_, C, _), _, nonvar(C)).
evaluable_body_hook(var(V),     _, nonvar(V)).
evaluable_body_hook(nonvar(V),  _, nonvar(V)).
evaluable_body_hook(atomic(A),  _, nonvar(A)).
evaluable_body_hook(atom(A),    _, nonvar(A)).
evaluable_body_hook(is_list(A), _, (ground(A);is_list(A))).
evaluable_body_hook(number(A),  _, nonvar(A)).
evaluable_body_hook(float(A),   _, nonvar(A)).
evaluable_body_hook(integer(A), _, nonvar(A)).
evaluable_body_hook(clause(A, _),    _, (strip_module(A, M, B), atom(M), callable(B))).
evaluable_body_hook(clause(A, _, _), _, (strip_module(A, M, B), atom(M), callable(B))).
evaluable_body_hook(format(Out, Format, Args), _,
                    (compound(Out), nonvar(Format), ground(Args))).
evaluable_body_hook(sort(A, _), _, (is_list(A), maplist(nonvar, A))).
evaluable_body_hook(A==B, _, (A==B;A\=B)).
evaluable_body_hook(A=..B, _, (is_list(B),B=[E|_],atomic(E);atomic(A);compound(A))).

replace_goal_hook(retractall(_), _, true).
replace_goal_hook(retract(_),    _, true).
replace_goal_hook(assertz(_),    _, true).
replace_goal_hook(asserta(_),    _, true).
replace_goal_hook(assert( _),    _, true).
replace_goal_hook(erase(  _),    _, true).
replace_goal_hook(gtrace, _, true).
replace_goal_hook(write(_, _), _, true).
replace_goal_hook(writeq(_, _), _, true).
replace_goal_hook(writeln(_, _), _, true).
replace_goal_hook(write(_), _, true).
replace_goal_hook(writeq(_), _, true).
replace_goal_hook(writeln(_), _, true).
replace_goal_hook(write_term(_, _), _, true).
replace_goal_hook(write_term(_, _, _), _, true).
replace_goal_hook(nl, _, true).
replace_goal_hook(nl(_), _, true).
replace_goal_hook(call_ai(G), abstract_interpreter, G).
replace_goal_hook(eval_ai(G), abstract_interpreter, G).
replace_goal_hook(skip_ai(_), abstract_interpreter, true).
replace_goal_hook(V is A, _, (ground(A)->V is A; var(V))).
replace_goal_hook(nb_getval(A, V), _, ignore((nonvar(A), nb_current(A, V)))).
replace_goal_hook(nb_setarg(_, _, _), _, true).

replace_body_hook(with_value(G, _, _), context_values, G).
replace_body_hook(with_value(G, _, _, _), context_values, G).
replace_body_hook(with_context_value(G, _, _), context_values, G).
replace_body_hook(with_context_value(G, _, _, _), context_values, G).
replace_body_hook(with_context_values(G, _, _), context_values, G).
replace_body_hook(with_context_values(G, _, _, _), context_values, G).
replace_body_hook('$with_asr'( G, _), ctrtchecks, G).
replace_body_hook('$with_gloc'(G, _), ctrtchecks, G).
replace_body_hook('$with_ploc'(G, _), ctrtchecks, G).
replace_body_hook(intr_ai(G), _, G).

%!  call_ai(:Goal)
%
%   Calls Goal during abstract interpretation and execution

call_ai(G) :- call(G).

%!  eval_ai(Goal)
%
%   Calls Goal during abstract interpretation but ignore during execution

eval_ai(_).

%!  skip_ai(Goal)
%
%   Calls Goal during execution bug ignore during abstract interpretation

skip_ai(G) :- call(G).

%!  intr_ai(Goal)
%
%   Abstract interpret Goal but ignore during execution

intr_ai(_).

mod_qual(M, G as R, I:H as B:C) :- !,
    strip_module(M:G, N, H),
    predicate_property(N:H, implementation_module(I)),
    strip_module(M:R, A, C),
    predicate_property(A:C, implementation_module(B)).
mod_qual(M, G :- B, I:H :- B) :-
    strip_module(M:G, N, H),
    predicate_property(N:H, implementation_module(I)).
mod_qual(M, G, I:F/A) :-
    strip_module(M:G, N, F/A),
    functor(H, F, A),
    predicate_property(N:H, implementation_module(I)).

:- public
    default_on_error/1.

default_on_error(Error) :-
    print_message(error, Error),
    backtrace(40 ).

%!  abstract_interpreter(:Goal, :Abstraction, +Options, -State) is multi.
%
%   Abstract interpret :Goal, call(Abstraction, Literal, Body, State1, State2)
%   is called over each found Literal to get an abstraction of the body of such
%   literal.  For instance, if abstraction is: `abstraction(Literal, Body,
%   State, State) :- clause(Literal, Body)`, the abstract interpreter becomes a
%   typical prolog interpreter, although some optimizations makes that not
%   accurate.
%
%   Valid options are:
%
%     * location(Loc)
%     A location of the given Goal, used to report the location in case of error
%
%     * evaluable(Eval)
%     Eval is a term or a list of term of the form:
%
%       - M:G as Repl
%         if the literal being interpreted match with G, and M with the
%         implementation module of literal, then Repl is called.
%       - M:F/A
%         equivalent to M:G as R, where functor(R, F, A) succeeds.
%       - M:G :- Body
%         if the literal being interpreted match with G, and M with the
%         implementation module of literal, then continue with the
%         interpretation of Body.
%
%    * on_error(OnErr)
%    Calls call(OnErr, at_location(Loc, Error)) if Error is raised

abstract_interpreter(M:Goal, Abstraction, Options, State) :-
    option(location(Loc),   Options, context(toplevel, Goal)),
    option(evaluable(Eval), Options, []),
    option(on_error(OnErr), Options, abstract_interpreter:default_on_error),
    flatten(Eval, EvalL), % make it easy
    maplist(mod_qual(M), EvalL, MEvalL),
    abstract_interpreter_body(Goal, M, Abstraction,
                              state(Loc, MEvalL, M:OnErr, [], [], [], []),
                              State).

%!  abstract_interpreter(:Goal, :Abstraction, +Options) is multi.
%
%   Same as abstract_interpreter(Goal, Abstraction, Options, _)

abstract_interpreter(MGoal, Abstraction, Options) :-
    abstract_interpreter(MGoal, Abstraction, Options, _).

:- meta_predicate catch(2, ?, ?, ?, ?).
catch(DCG, Ex, H, S1, S) :-
    catch(call(DCG, S1, S), Ex, H).


:- meta_predicate cut_to(2, ?, ?).

%!  cut_to(:Goal, ?State0, ?State)
%
%   Mark a place to where cut_from will come back.

cut_to(Goal) --> catch(Goal, cut_from, true).

%!  cut_from.
%
%   Jump to where the cut_to was called

cut_from.
cut_from :- throw(cut_from).

/*
% alternative (and more efficient) implementation follows:
% Note: this does not work since the choice points could be removed
% by a further cut operation, causing odd behavior
%
:- use_module(library(intercept)).
:- use_module(library(safe_prolog_cut_to)).

:- meta_predicate intercept(2, ?, ?, ?, ?).
intercept(DCG, Ex, H, S1, S) :-
    intercept(call(DCG, S1, S), Ex, H).

cut_to(Goal) -->
    {prolog_current_choice(CP)},
    intercept(Goal, cut_from, catch(safe_prolog_cut_to(CP), _, true)).

cut_from :- send_signal(cut_from).
*/

%!  bottom(State1, State) is det.
%
%   Sets the state of  the analysis to bottom, which means  that the analysis is
%   unable to determine  a solution to the Goal (universe  set).  Note that this
%   could  be due  to  a lack  of  precision  of the  analysis  or simply  being
%   mathematically impossible to get a solution statically.

bottom(state(Loc, EvalL, OnErr, CallL, D, Cont, _),
       state(Loc, EvalL, OnErr, CallL, D, Cont, bottom)).

%!  abstract_interpreter_body(+Goal, +M, :Abstraction, ?State1, ?State) is multi.
%
%   Like abstract_interpret(M:Goal,  Abstraction, Options, State),  where State1
%   is determined using  Options, but intended to be  called recursivelly during
%   the interpretation.

abstract_interpreter_body(Goal, M, _) -->
    {var(Goal) ; var(M)}, bottom, !.
abstract_interpreter_body(M:Goal, _, Abs) -->
    !,
    abstract_interpreter_body(Goal, M, Abs).
abstract_interpreter_body(@(M:Goal, CM), _, Abs) -->
    !,
    cut_to(abstract_interpreter_lit(Goal, M, CM, Abs)).

abstract_interpreter_body(call(Goal), M, Abs) --> !,
    cut_to(abstract_interpreter_body(Goal, M, Abs)).
abstract_interpreter_body(\+ A, M, Abs) --> !,
    abstract_interpret_body_not(A, M, Abs).

abstract_interpret_body_not(A, M, Abs) -->
    ( cut_to(abstract_interpreter_body(A, M, Abs))
    ->( \+ is_bottom
      ->!,
        {fail}
      ; {fail}
      )
    ; !
    ).
abstract_interpret_body_not(_, _, _) --> bottom.

get_conts(Conts, State, State) :-
    State = state(_, _, _, _, _, Conts, _).

put_conts(Conts,
          state(Loc, EvalL, OnErr, CallL, Data, _, Result),
          state(Loc, EvalL, OnErr, CallL, Data, Conts, Result)).

abstract_interpreter_body(catch(Goal, Ex, Handler), M, Abs, S1, S) :-
    !,
    catch(abstract_interpreter_body(Goal, M, Abs, S1, S), Ex,
          ( Handler,
            S = S1
          )).
abstract_interpreter_body(once(Goal), M, Abs, S1, S) :- !,
    once(abstract_interpreter_body(Goal, M, Abs, S1, S)).
abstract_interpreter_body(distinct(Goal), M, Abs, S1, S) :-
    predicate_property(M:distinct(_), implementation_module(solution_sequences)), !,
    distinct(Goal, abstract_interpreter_body(Goal, M, Abs, S1, S)).
abstract_interpreter_body(distinct(Witness, Goal), M, Abs, S1, S) :-
    predicate_property(M:distinct(_, _), implementation_module(solution_sequences)), !,
    distinct(Witness, abstract_interpreter_body(Goal, M, Abs, S1, S)).

ord_spec(asc(_)).
ord_spec(desc(_)).

abstract_interpreter_body(order_by(Spec, Goal), M, Abs, S1, S) :- !,
    ( is_list(Spec),
      Spec \= [],
      maplist(nonvar, Spec),
      maplist(ord_spec, Spec)
    ->order_by(Spec, abstract_interpreter_body(Goal, M, Abs, S1, S))
    ; abstract_interpreter_body(Goal, M, Abs, S1, S)
    ).
abstract_interpreter_body(setup_call_cleanup(S, C, E), M, Abs, State1, State) :- !,
    setup_call_cleanup(abstract_interpreter_body(S, M, Abs, State1, State2),
                       abstract_interpreter_body(C, M, Abs, State2, State3),
                       abstract_interpreter_body(E, M, Abs, State3, State)),
    ignore((var(State), State = State2)).
abstract_interpreter_body(call_cleanup(C, E), M, Abs, State1, State) :- !,
    call_cleanup(abstract_interpreter_body(C, M, Abs, State1, State2),
                 abstract_interpreter_body(E, M, Abs, State2, State)),
    ignore((var(State), State = State2)).
abstract_interpreter_body((A, B), M, Abs) -->
    !,
    { \+ terms_share(A, B)
    ->CutOnFail = true
    ; CutOnFail = fail
    },
    get_conts(ContL),
    put_conts([B|ContL]),
    abstract_interpreter_body(A, M, Abs),
    put_conts(ContL),
    ( abstract_interpreter_body(B, M, Abs)
    *->[]
    ; { CutOnFail = true
      ->!, fail                 % The whole body will fail
      }
    ).
abstract_interpreter_body((A*->B;C), M, Abs) -->
    !,
    { \+ terms_share(A, B)
    ->CutOnFail = true
    ; CutOnFail = fail
    },
    ( get_conts(ContL),
      put_conts([B|ContL]),
      abstract_interpreter_body(A, M, Abs),
    % *->
      put_conts(ContL),
      ( abstract_interpreter_body(B, M, Abs)
      *->[]
      ; { CutOnFail = true
        ->!, fail                 % The whole body will fail
        }
      )
    ; abstract_interpreter_body(C, M, Abs)
    ).
abstract_interpreter_body((A->B;C), M, Abs) --> !,
    {SCE = s(no)},
    ( interpret_local_cut(A, B, M, Abs, CutElse),
      {nb_setarg(1, SCE, CutElse)}
    ; ( {SCE = s(no)}
      ->abstract_interpreter_body(C, M, Abs)
      )
    ).
abstract_interpreter_body((A;B), M, Abs) --> !,
    ( abstract_interpreter_body(A, M, Abs)
    ; abstract_interpreter_body(B, M, Abs)
    ).
abstract_interpreter_body(A->B, M, Abs) --> !,
    interpret_local_cut(A, B, M, Abs, _).
abstract_interpreter_body(CallN, M, Abs) -->
    {do_resolve_calln(CallN, Goal)}, !,
    cut_to(abstract_interpreter_body(Goal, M, Abs)).

push_top(Prev,
         state(Loc, EvalL, OnErr, CallL, Data, Cont, Prev),
         state(Loc, EvalL, OnErr, CallL, Data, Cont, [])).

pop_top(bottom,
        state(Loc, EvalL, OnErr, CallL, Data, Cont, _),
        state(Loc, EvalL, OnErr, CallL, Data, Cont, bottom)).
pop_top([]) --> [].

% CutElse make the failure explicit wrt. B
interpret_local_cut(A, B, M, Abs, CutElse) -->
    { \+ terms_share(A, B)
    ->CutOnFail = true
    ; CutOnFail = fail
    },
    push_top(Prev),
    get_conts(ContL),
    put_conts([B|ContL]),
    cut_to(abstract_interpreter_body(A, M, Abs)), % loose of precision
    put_conts(ContL),
    ( \+ is_bottom
    ->!,
      { CutElse = yes }
    ; { CutElse = no  }
    ),
    pop_top(Prev),
    ( abstract_interpreter_body(B, M, Abs)
    *->
      []
    ; ( {CutOnFail = true}
      ->cut_if_no_bottom
      ; []
      )
    ).
abstract_interpreter_body(!,    _, _) --> !, cut_if_no_bottom.
abstract_interpreter_body(A=B,  _, _) --> !, {A=B}.
abstract_interpreter_body(A\=B, _, _) -->
    !,
    ( {A\=B}
    ->[]
    ; {A==B}
    ->{fail}
    ; bottom
    ).
abstract_interpreter_body(BinExpr, _, _) -->
    { member(BinExpr, [A=\=B,
                       A=:=B,
                       A>B,
                       A<B,
                       A>=B,
                       A=<B])
    },
    necki,
    !,
    ( { ground(A),
        ground(B)
      }
    ->{BinExpr}
    ; bottom
    ).
abstract_interpreter_body(memberchk(A, B), _, _) -->
    !,
    ( {is_list(B)}
    ->( {nonvar(A)}
      ->{memberchk(A, B)}
      ; {member(A, B)},
        bottom
      )
    ; { append(_, [A|T], B),
        ( var(T)
        ->!
        ; true
        )
      },
      bottom
    ).
abstract_interpreter_body(true, _, _) --> !.
abstract_interpreter_body(fail, _, _) --> !, {fail}.
abstract_interpreter_body(A, M, _) -->
    get_state(state(Loc, _, OnError, CallL, _, _, _)),
    {evaluable_body_hook(A, M, Condition)},
    !,
    ( {call(Condition)}
    ->{catch(M:A,
             Error,
             ( call(OnError, at_location(Loc, Error)),
               call(OnError, call_stack(CallL)),
               fail
             ))
      }
    ; bottom
    ).

abstract_interpreter_body(G, M, _) -->
    get_state(state(_, EvalL, _, _, _, _, _)),
    {get_body_replacement(G, M, EvalL, MR)},
    !,
    {call(MR)}.
abstract_interpreter_body(H, M, Abs) -->
    cut_to(abstract_interpreter_lit(H, M, M, Abs)).

get_body_replacement(G, M, EvalL, MR) :-
    predicate_property(M:G, implementation_module(IM)),
    ( ( evaluable_goal_hook(G, IM)
      ; functor(G, F, A),
        memberchk(IM:F/A, EvalL)
      ),
      MR = M:G
    ; replace_goal_hook(G, IM, R),
      MR = M:R
    ; copy_term(EvalL, EvalC),
      memberchk((IM:G as I:R), EvalC),
      % This weird code is used because we can not use @/2 here
      qualify_meta_goal(M:R, MQ),
      strip_module(MQ, _, Q),
      MR = I:Q
    ).

is_bottom(State, State) :-
    State = state(_, _, _, _, _, _, bottom),
    neck.

cut_if_no_bottom -->
    ( \+ is_bottom
    ->{cut_from}
    ; []
    ).

%!  get_state(State, State, State).
%
%   Used in DCG's to get the current State

get_state(State, State, State).

%!  put_state(State, _, State).
%
%   Used in DCG's to set the current State

put_state(State, _, State).

abstract_interpreter_lit(H, M, CM, Abs) -->
    { predicate_property(M:H, meta_predicate(Meta))
    ->qualify_meta_goal(CM:H, Meta, Goal)
    ; Goal = H
    },
    {predicate_property(M:Goal, implementation_module(IM))},
    get_state(state(Loc, EvalL, OnError, CallL, Data, Cont, Result)),
    ( {member(MCall-_, CallL),
       MCall =@= IM:Goal
      }
    ->bottom
    ; {copy_term(IM:Goal, MCall)},
      put_state(state(Loc, EvalL, OnError, [MCall-Loc|CallL], Data, Cont, Result)),
      ( { copy_term(EvalL, EvalC), % avoid undesirable unifications
          memberchk((IM:Goal :- Body), EvalC)
        ; replace_body_hook(Goal, IM, Body)
        }
      ->cut_to(abstract_interpreter_body(Body, M, Abs))
      ; { \+ predicate_property(M:Goal, defined) }
      ->{ call(OnError, error(existence_error(procedure, M:Goal), Loc)),
          call(OnError, call_stack(CallL)),
          % TBD: information to error
          fail
        }
      ; call(Abs, M:Goal, BM:Body),
        cut_to(abstract_interpreter_body(Body, BM, Abs))
      )
    ).

%!  match_head(:Goal, :Body, ?State1, ?State) is multi.
%
%   Implements the next abstraction: Only test matches of literals with heads of
%   clauses, without digging into the body.

match_head(MGoal, M:true) -->
    {predicate_property(MGoal, interpreted)},
    !,
    {strip_module(MGoal, M, _)},
    get_state(state(_,   EvalL, OnErr, CallL, D, Cont, Result)),
    put_state(state(Loc, EvalL, OnErr, CallL, D, Cont, Result)),
    {match_head_body(MGoal, Body, Loc)},
    ( {Body = _:true}
    ->[]
    ; bottom %% loose of precision
    ).
match_head(MGoal, M:true) -->
    {strip_module(MGoal, M, _)},
    bottom.

%!  match_head_body(:Goal, -Body, -From) is  multi.
%
%   Auxiliar  predicate  used to  implement  some  abstractions. Given  a  Goal,
%   unifies  Body with  the body  of the  matching clauses  and From  with the
%   location of the clause.

match_head_body(MGoal, CMBody, From) :-
    ( extra_clauses(MGoal, CMBody, From)
    ; From = clause(Ref),
      clause(MGoal, Body, Ref),
      clause_property(Ref, module(CM)),
      CMBody = CM:Body
    ).

extra_clauses(M:Goal, Body, From) :-
    extra_clauses(Goal, M, Body, From).

%!  extra_clauses(Goal, Module, :Body, -From) is multi.
%
%   Called   inside  match_head_body/3   to  increase   the  precision   of  the
%   interpreter, it will define 'semantic' extra clauses, allowing for instance,
%   analysis of dynamic predicates, interfaces, etc.

:- multifile extra_clauses/4.

extra_clauses(Goal, CM, I:Goal, _From) :-
    predicate_property(CM:Goal, implementation_module(M)),
    functor(Goal, F, A),
    ( interface:'$interface'(M, DIL),
      memberchk(F/A, DIL)
    ->interface:'$implementation'(I, M)
    ).

%!  match_noloops(:Goal, :Body, ?State1, ?State) is multi.
%
%   Implements the next abstraction: Only test matches of literals with heads of
%   clauses, and digs into the body provided that there are not recursive calls,
%   in such a case the analysis reach bottom and we stop the recursion.

match_noloops(MGoal, Body) -->
    {predicate_property(MGoal, interpreted)},
    !,
    {strip_module(MGoal, M, Goal)},
    get_state(state(Loc1, EvalL, OnErr, CallL, S, Cont, Result1)),
    { functor(Goal, F, A),
      term_size(Goal, Size),
      \+ ( memberchk(M:F/A-Size1, S),
           Size1=<Size
         )
    ->match_head_body(MGoal, Body, Loc),
      Result = Result1
    ; Loc = Loc1,
      Result = bottom %% loose of precision
    },
    put_state(state(Loc, EvalL, OnErr, CallL, [M:F/A-Size|S], Cont, Result)).
match_noloops(MGoal, M:true) -->
    {strip_module(MGoal, M, _)},
    bottom.
