/*  Andorra

    Author:        The Ciao Development Team, ported by Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           http://www.edisonm.com
    Copyright (C): 2019, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(andorra_rt,
          [ wakeup/2,
	    suspend_andorra/5,
	    obtain_vars/2,
	    simplify/2,
            verify_det/3,
            determinate/2,
            detcond/1
	  ]).

:- use_module(library(apply)).
:- use_module(library(andorra_op)).

:- dynamic '$read_mode'/1.

:- meta_predicate
    suspend_andorra(?,?,0,?,?),
    simplify(0, -).

obtain_vars((C1,_C2),Vars):-
    obtain_vars(C1,Vars).
obtain_vars((C1;C2),Vars):-
    obtain_vars(C1,Vars1),
    obtain_vars(C2,Vars2),
    ord_union(Vars1,Vars2,Vars).
obtain_vars(Cond,Vars):-
    term_variables(Cond,Vars).

suspend_andorra(S,LS,HSusp,Cond,Type):-
    put_attr(S, andorra_rt, frozen_andorra(LS,HSusp,Cond,Type,S)),
    prepare_det(LS,S).

set_read_mode(Value) :-
    retractall('$read_mode'(_)),
    assertz('$read_mode'(Value)).

verify_det(HOrig,LGyCs,NewLGyCs):-
    set_read_mode(yes),
    update_state(HOrig,LGyCs,NewLGyCs),
    set_read_mode(no).

wake_determinate_(_):-
    '$read_mode'(yes),
    !.
wake_determinate_(Lg):-
    maplist(wake_determinate, Lg).

update_state(_,[],[]):- !.
update_state(HOrig,[guard_clause(H,R)|LGyCs],[guard_clause(H,R)|NewLGyCs]):-
    \+ \+ HOrig = H,
    valid(R),
    !,
    update_state(HOrig,LGyCs,NewLGyCs).

update_state(HOrig,[guard_clause(_H,_R)|LGyCs],NewLGyCs):-
    update_state(HOrig,LGyCs,NewLGyCs).

valid([]) :- !.
valid([Const|Rest]):-
    ( ground(Const)
    ->Const
    ; true
    ),
    valid(Rest).

test_unify(X,Y) :- 
    set_read_mode(yes),
    \+ \+ X = Y,
    set_read_mode(no).
test_unify(_X,_Y) :-
    set_read_mode(no),
    fail.

wakeup(L1,L2) :- L1==L2, !.
wakeup([L1|L2],L3) :-
    L1=up, 
    wakeup(L2,L3).

prepare_det([],_).
prepare_det([V|Rest],S) :-
    (   get_attr(V, andorra_rt, affect_det(Sset,V))
    ->  ord_add_element(Sset,S,NewSset),
	put_attr(V, andorra_rt, affect_det(NewSset,V))
    ;   put_attr(V, andorra_rt, affect_det([S],V))
    ),
    prepare_det(Rest,S).

attr_unify_hook(frozen_andorra(LS,HSusp,_HOrig,_LGyCs,S),V) :-
    V==up,
    !,
    remove_affect(LS,S),
    call(HSusp).
attr_unify_hook(affect_det(Lg1,_), V) :-
    ( nonvar(V)
    ->wake_determinate_(Lg1)
    ; get_attr(V, andorra_rt, affect_det(Lg2,_)),
      ord_union(Lg1, Lg2, Lg),
      reduce_to_set(Lg),
      put_attr(V, andorra_rt, affect_det(Lg, V))
    ).

reduce_to_set([]).
reduce_to_set([S|Rest]):-
    get_attr(S, andorra_rt, frozen_andorra(LS,HSusp,HOrig,LGyCs,S)),
    sort(LS, SortedLS),
    put_attr(S, andorra_rt, frozen_andorra(SortedLS,HSusp,HOrig,LGyCs,S)),
    reduce_to_set(Rest).

wake_determinate(S) :-
    ( get_attr(S, andorra_rt, frozen_andorra(LS,HSusp,HOrig,LGyCs,S))
    ->( LGyCs = builtin
      ->simplify(HOrig, NewCond),
	update_condition_builtin(S,LS,HSusp,NewCond)
      ; verify_det(HOrig,LGyCs,NewLGyCs),
	update_condition(LS,HSusp,HOrig,NewLGyCs,S)
      )
    ; true
    ).

update_condition_builtin(S,LS,HSusp,NewCond):-
	( 
	    NewCond = true ->
	    del_attr(S, andorra_rt),
	    remove_affect(LS,S),	                
	    !,call(HSusp) 
	; 
	    reduce_attributes_builtin(S,LS,HSusp,NewCond,builtin)
	).


update_condition(LS,HSusp,HOrig,NewLGyCs,S) :-
	( 
	    NewLGyCs = [] -> 
	    del_attr(S, andorra_rt),   
	    remove_affect(LS,S),	                
	    !,fail 
	;
	    ( 
		NewLGyCs = [_] -> 
		del_attr(S, andorra_rt),
		remove_affect(LS,S),	                
		!,
		call(HSusp)
	    ;
		reduce_attributes(S,LS,HSusp,HOrig,NewLGyCs)
	    )
	).

reduce_attributes_builtin(S,LS,HSusp,HOrig,LGyCs) :-
	obtain_vars(HOrig,NewLS),!,
	put_attr(S, andorra_rt, frozen_andorra(NewLS,HSusp,HOrig,LGyCs,S)),
	sort(LS,OldLS),
	ord_intersection(OldLS,NewLS,I,OldVars),
	ord_intersection(NewLS,OldLS,I,Vars),
	remove_affect(OldVars,S),
	maplist(add_affect(S), Vars).


reduce_attributes(S,LS,HSusp,HOrig,LGyCs):-
	term_variables(HOrig,NewLS),
	put_attr(S, andorra_rt, frozen_andorra(NewLS,HSusp,HOrig,LGyCs,S)),
	sort(LS,OldLS),
	ord_intersection(OldLS,NewLS,I,OldVars),
	ord_intersection(NewLS,OldLS,I,Vars),
	remove_affect(OldVars,S),
	maplist(add_affect(S), Vars).

add_affect(S, V) :-
    ( get_attr(V, andorra_rt, affect_det(Sset,V))
    ->ord_add_element(Sset,S,NewSset),
      put_attr(V, andorra_rt, affect_det(NewSset,V))
    ; var(V)
    ->put_attr(V, andorra_rt, affect_det([S],V))
    ; true
    ).

remove_affect([],_S).
remove_affect([V|Vs],S):-
	(
            var(V) ->
            get_attr(V, andorra_rt, affect_det(Lg,V)),
            ord_del_element(Lg,S, NewLg),
            (
                NewLg = [] ->
                del_attr(V, andorra_rt)
            ;
                put_attr(V, andorra_rt, affect_det(NewLg,V))
            )
        ;
            true
        ),
        remove_affect(Vs,S).


getterm(A,A):-
	var(A),!,fail.
getterm(term(A,Path),C):- 
	!, 
	getterm_(A,Path,C).
getterm(A,A).


getterm_(Term,[],Term):- 
	nonvar(Term),!.
getterm_(Term,[N|Path],SubTerm):- 
	nonvar(Term),
	functor(Term,_,A),
	(
	    A>=N ->
	    arg(N,Term,Arg),
	    getterm_(Arg,Path,SubTerm)
	;
	    SubTerm = '$missing'
	).
  
	
instantiated(Term,[],'$nonvar'):- nonvar(Term).
instantiated(Term,[N|Path],Up):-
	nonvar(Term),
	functor(Term,_,A),
	A>=N,
	arg(N,Term,Arg),
	instantiated(Arg,Path,Up).
instantiated(Term,Path,instantiated(Term,Path)).

simplify(M:G, S) :-
    simplify(G, M, S).

simplify(Var, M, M:Var) :-
    var(Var),
    !.
simplify(G, M, S) :-
    simplify2(G, M, S),
    !.
simplify(C, M, true):-
    call(M:C),
    !.
simplify(_, _, false).

simplify2(M:G, _, S) :- simplify(G, M, S).
simplify2(true, _, true).
simplify2(ground(Term), _, NewTerm) :-
    ( ground(Term)
    ->NewTerm = true
    ; term_variables(Term, AllVars),
      conjunction(AllVars, NewTerm)
    ).
simplify2(nonvar(Term), _, NewTerm) :-
    ( nonvar(Term)
    ->NewTerm = true
    ; NewTerm = nonvar(Term)
    ).
simplify2(A ?= B, _, Simplified) :-
    ( getterm(A,TermA)
    ->( getterm(B,TermB)
      ->( test_unify(TermA,TermB)
        ->Simplified = true
	; Simplified = false
	)
      ; Simplified = (A ?= B)
      )
    ; Simplified = (A ?= B)
    ).
simplify2(A ?\= B, _, Simplified) :-
    ( getterm(A,TermA)
    ->( getterm(B,TermB)
      ->( \+ test_unify(TermA,TermB)
        ->Simplified = true
	; Simplified = (TermA ?\= TermB)
          % Simplified = false
	)
      ; Simplified = (A ?\= B)
      )
    ; Simplified = (A ?\= B)
    ).
simplify2(instantiated(A,Path), _, Result) :-
    instantiated(A,Path,Term),
    ( Term = '$nonvar'
    ->Result = true
    ; Result = Term
    ),
    !.
simplify2(A \== B, _, S) :-
    ( A \== B
    ->S = true
    ; S = false
    ).
simplify2(A == B, _, S) :-
    ( A == B
    ->S = true
    ; S = false
    ).
simplify2(false, _, false).
simplify2((C1, C2), M, Result) :-
    simplify(C1, M, R1),
    ( R1 = false
    ->Result = false
    ; ( R1 = true
      ->simplify(C2, M, Result)
      ; Result = (C1,C2)
      )
    ).
simplify2((C1; C2), M, Result) :-
    simplify(C1, M, R1),
    ( R1 = true
    ->Result = true
    ; simplify(C2, M, R2),
      or(R1, R2, Result)
    ).

conjunction([G], ground(G)) :- !.
conjunction([V|Vs], (ground(V), Rest)) :- 
    conjunction(Vs, Rest).

%or(true, true, true):- !.
or(C1, false, C1):- !.
or(false, C2, C2):- !.
or(_C1, true, true):- !.
%or(true, _C2, true):- !.
or(C1, C2, (C1; C2)).

%!  determinate(Pred, Cond:detcond)

%   Declares determinacy conditions for a
%   predicate. Conditions @var{Cond} are on variables of arguments
%   of @var{Pred}. For example, in:
%
%       :- determinate(member(A,B,C), ( A ?\= term(B,[1])  ; C?\=[_|_]) ).
%
%       member(A,[A|B],B).
%       member(A,[B|C],[B|D]) :-
%               A\==B,
%               member(A,C,D).
%
%   the declaration states that a call @tt{member(A,B,C)} is
%   determinate when either @tt{A} doesn't unify with the first
%   argument of @tt{B} or @tt{C} doesn't unify with @tt{[_|_]}."

determinate(Pred, Cond) :-
    print_message(error, format("~w should not be called directly, but in a declaration",
                                [determinate(Pred, Cond)])),
    fail.

%!  detcond(X)

%   X is a determinacy condition.
%
%  - ground/1 and @tt{nonvar/1} have the usual meaning.
%  - instatiated(A,Path) means that the subterm of A
%    addressed by Path is not a variable. Path is a 
%    list of integer numbers describing a path to the subterm 
%    regarding the whole term A as a tree. For example,
%    instantiated(f(g(X),h(i(Z),Y)),[2,1]) tests whether 
%    i(Z) is not a variable. 
%  - Term1 ?\\= Term2 means ``terms Term1 and Term2
%    do not unify (when instantiated)''. Term1 and Term2
%    can be either an argument of the predicate or a term 
%    term(V,Path), which
%    refers to the subterm of @tt{V} addressed by @tt{Path}. 
%  - Term1 ?= Term2 means ``terms Term1 and Term2
%    unify (when instantiated)''. The same considerations above
%    apply to @tt{Term1} and @tt{Term2}.
%  - any other test that does not unify variables can also be used
%    ( ==/2, \\==/2, atomic/1 ).

detcond(ground(X)):- var(X).
detcond(nonvar(X)):- var(X).
detcond(instatiated(A,Path)):- var(A), maplist(int,Path).
detcond(Term1 ?\= Term2):- path(Term1), path(Term2).
detcond(Term1 ?= Term2):- path(Term1), path(Term2).
detcond(Test):- test(Test).

test(_).

path(X):- var(X).
path(X):- maplist(int,X).
