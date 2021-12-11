:-module(heuristics,[]).

early_precedence(queen, 1).
early_precedence(beetle, 3).
early_precedence(grasshoper, 1).
early_precedence(spider,2).
early_precedence(ant, 1).
early_precedence(ladybug, 1).
early_precedence(mosquito, 3).
early_precedence(pigbull,1).


mid_precedence(queen, 3).
mid_precedence(beetle, 2).
mid_precedence(grasshoper, 3).
mid_precedence(spider,2).
mid_precedence(ant, 1).
mid_precedence(ladybug, 3).
mid_precedence(mosquito, 1).
mid_precedence(pigbull,2).


early_comparer(>, [T1|_], [T2|_]):-
    early_precedence(T1, P1),
    early_precedence(T2, P2),
    P1 > P2.

early_comparer(<, [T1|_], [T2|_]):-
    early_precedence(T1, P1),
    early_precedence(T2, P2),
    P1 =< P2.

mid_comparer(>, [T1|_], [T2|_]):-
    early_precedence(T1, P1),
    early_precedence(T2, P2),
    P1 > P2.

mid_comparer(<, [T1|_], [T2|_]):-
    mid_precedence(T1, P1),
    mid_precedence(T2, P2),
    P1 =< P2.


placedBug(Color, Type, Val):-
    \+ board:bug(Color, Type, _,_,_), Val = 0, !;
    Val = 1.

vFormation(Color, Val):-
    \+ (board:bug(Color, queen, X, Y, _),
    board:adyacentAlly(X,Y,X1,Y1,Color),
    board:adyacentAlly(X,Y,X2,Y2,Color),
    board:adyacent(X1,Y1,X2,Y2)),
    Val = 0, !;
    Val = 1.




eval(Color, Val):-
    board:currentTurn(Color, Turn),
    Turn =< 4,

    placedBug(Color, queen, QS),
    placedBug(Color, grasshoper, GS),
    placedBug(Color, ant, AS),
    placedBug(Color, ladybug, LS),
    placedBug(Color, pigbull, PS),
    vFormation(Color, VS),
    random(R),

    Val is QS + GS + AS + LS + PS + VS - R.











