:- begin_tests(clpcd).

:- [mip].

test(entailed) :-
    {A =< 4},
    entailed(A=\=5).

test(entailed) :-
    {A =< 4},
    \+ entailed(A=\=3).

test(sup) :-
    { 2*X+Y=<16,
      X+2*Y=<11,
      X+3*Y=<15,
      Z=30*X+50*Y
    },
    sup(Z,Sup),
    assertion({Sup = 310}).

test(inf) :-
    { 2*X+Y=<16,
      X+2*Y=<11,
      X+3*Y=<15,
      Z= -30*X-50*Y
    },
    inf(Z,Inf),
    assertion({Inf = -310}).

test(maximize) :-
    { 2*X+Y=<16,
      X+2*Y=<11,
      X+3*Y=<15,
      Z=30*X+50*Y
    },
    maximize(Z),
    assertion({Z = 310,
               Y = 2,
               X = 7}).

test(infe) :-
    example(flugpl, Obj, _, _, _),
    inf(Obj,Inf),
    assertion({Inf = 11429082625/9792}).

test(supe) :-
    example(flugpl, Obj, _, _, _),
    sup(-Obj, Inf),
    assertion({Inf = -11429082625/9792}).

test(bb_inf) :-
    example(flugpl,Obj,_,Ints,_),
    bb_inf(Ints, Obj, Inf),
    assertion({Inf = 1201500}).

:- end_tests(clpcd).
