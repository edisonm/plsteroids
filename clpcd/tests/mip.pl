
example(flugpl, Obj, Vs, Ints, []) :-
    Vs = [Anm1, Anm2, Anm3, Anm4, Anm5, Anm6,
          Stm1, Stm2, Stm3, Stm4, Stm5, Stm6,
          UE1, UE2, UE3, UE4, UE5, UE6],
    Ints = [Stm6, Stm5, Stm4, Stm3, Stm2,
            Anm6, Anm5, Anm4, Anm3, Anm2, Anm1],

    Obj
    = 2700*Stm1 + 1500*Anm1 + 30*UE1
    + 2700*Stm2 + 1500*Anm2 + 30*UE2
    + 2700*Stm3 + 1500*Anm3 + 30*UE3
    + 2700*Stm4 + 1500*Anm4 + 30*UE4
    + 2700*Stm5 + 1500*Anm5 + 30*UE5
    + 2700*Stm6 + 1500*Anm6 + 30*UE6,
    allpos(Vs),
    { Stm1 = 60, 9r10*Stm1+1*Anm1-1*Stm2 = 0 },
    { 9r10*Stm2+1*Anm2-1*Stm3=0 },
    { 9r10*Stm3+1*Anm3-1*Stm4=0 },
    { 9r10*Stm4+1*Anm4-1*Stm5=0 },
    { 9r10*Stm5+1*Anm5-1*Stm6=0 },
    { 150*Stm1 -100*Anm1 +1*UE1 >= 8000 },
    { 150*Stm2 -100*Anm2 +1*UE2 >= 9000 },
    { 150*Stm3 -100*Anm3 +1*UE3 >= 8000 },
    { 150*Stm4 -100*Anm4 +1*UE4 >= 10000 },
    { 150*Stm5 -100*Anm5 +1*UE5 >= 9000 },
    { 150*Stm6 -100*Anm6 +1*UE6 >= 12000 },
    { -20*Stm1 +1*UE1 =< 0 },
    { -20*Stm2 + 1*UE2 =< 0 },
    { -20*Stm3 +1*UE3 =< 0 },
    { -20*Stm4 +1*UE4 =< 0 },
    { -20*Stm5 + 1*UE5 =< 0 },
    { -20*Stm6 +1*UE6 =< 0 },
    { Anm1 =< 18 },
    { 57 =< Stm2 },
    { Stm2 =< 75 },
    { Anm2 =< 18 },
    { 57 =< Stm3 },
    { Stm3 =< 75 },
    { Anm3 =< 18 },
    { 57 =< Stm4 },
    { Stm4 =< 75 },
    { Anm4 =< 18 },
    { 57 =< Stm5 },
    { Stm5 =< 75 },
    { Anm5 =< 18 },
    { 57 =< Stm6 },
    { Stm6 =< 75 },
    { Anm6 =< 18 },
    true.

allpos([]).
allpos([X|Xs]) :- {X >= 0}, allpos(Xs).

user:portray(N) :-
    ( rational(N),
      \+ integer(N)
    ),
    format('"~12gq"', [N]).
