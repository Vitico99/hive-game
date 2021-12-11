:-module(heuristics,[]).

earlyPrecedence(queen, 1).
earlyPrecedence(beetle, 3).
earlyPrecedence(grasshoper, 1).
earlyPrecedence(spider,2).
earlyPrecedence(ant, 1).
earlyPrecedence(ladybug, 1).
earlyPrecedence(mosquito, 3).
earlyPrecedence(pigbull,1).


midPrecedence(queen, 2).
midPrecedence(beetle, 2).
midPrecedence(grasshoper, 3).
midPrecedence(spider,1).
midPrecedence(ant, 1).
midPrecedence(ladybug, 3).
midPrecedence(mosquito, 1).
midPrecedence(pigbull,2).


earlyOrdering(>, [T1|_], [T2|_]):-
    earlyPrecedence(T1, P1),
    earlyPrecedence(T2, P2),
    P1 > P2.

earlyOrdering(<, [T1|_], [T2|_]):-
    earlyPrecedence(T1, P1),
    earlyPrecedence(T2, P2),
    P1 =< P2.

midOrdering(>, [T1|_], [T2|_]):-
    midPrecedence(T1, P1),
    midPrecedence(T2, P2),
    P1 > P2.

midOrdering(<, [T1|_], [T2|_]):-
    midPrecedence(T1, P1),
    midPrecedence(T2, P2),
    P1 =< P2.


getOrderingHeuristic(Color, heuristics:earlyOrdering):-
    board:currentTurn(Color, Turn), Turn =< 4, !.
getOrderingHeuristic(Color, heuristics:midOrdering):-
   board:currentTurn(Color, Turn), Turn > 4, aggregate_all(count, board:bug(Color,_,_,_,_), Cnt), Cnt < 12, !.


queenSurrounded(Color, 0):- \+ board:bug(Color, queen, _,_,_).
queenSurrounded(Color, Count):-
    board:bug(Color, queen, X1,Y1,0),
    !,
    aggregate_all(count, board:nonEmptyAdyacent(X1,Y1,X2,Y2), Count).


pinnedBug(C,T):-
    board:bug(C,T,X,Y,S1),
    board:getCellTop(X,Y,S2),
    S1 < S2.
pinnedBug(C,T):-
    board:bug(C,T,X,Y,S),
    (\+board:canBeRemoved(X,Y);
    \+board:canBeMoved(X,Y,S)).


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

pinnedQueenMetric(Color, Val):-
    pinnedBug(Color, queen),
    Val = 1, !;
    Val = 0.

pinnedAntsMetric(Color, Val):-
    aggregate_all(count, pinnedBug(Color, ant), Val).

bugCountMetric(Color, Type, Val):-
    aggregate_all(count, board:bug(Color, Type, _,_,_), Val).

mosquitoLikeAnt(Color):-
    board:bug(Color, mosquito, X, Y, 0),
    board:getCellTop(X,Y,0),
    board:adyacent(X,Y,X1,Y1),
    board:bug(_,ant,X1,Y1,S),
    board:getCellTop(X1,Y1,S).



% =============================================== Early Game ================================================


eval(Color, Val):-
    board:currentTurn(Color, Turn),
    Turn =< 4, !,

    placedBug(Color, queen, QS),
    placedBug(Color, grasshoper, GS),
    placedBug(Color, ant, AS),
    placedBug(Color, ladybug, LS),
    placedBug(Color, pigbull, PS),
    vFormation(Color, VS),
    pinnedAntsMetric(Color, PA),
    random(R),

    Val is QS + GS + AS + LS + PS + VS - PA - R.

eval(Color, Val):-
    board:currentTurn(Color, Turn),
    Turn > 4,
    aggregate_all(count, board:bug(Color,_,_,_,_), Cnt),
    Cnt < 12,
    
    board:opponent(Color, OColor),

    bugCountMetric(Color, ant, AntCount),
    pinnedAntsMetric(Color, PinnedAnts),
    FreeAnts is AntCount - PinnedAnts,
    pinnedAntsMetric(Ocolor, OPinnedAnts),  

    (\+mosquitoLikeAnt(Color), MosquitoAnt is 1; MosquitoAnt is 0),

    bugCountMetric(Color, spider, SpiderCount),
    pinnedQueenMetric(OColor, PinnedQueen),
    queenSurrounded(Color, CQueenSurround),
    queenSurrounded(Ocolor, OQueenSurround),


    Val is 150 * FreeAnts + 350 * OPinnedAnts + 150 * MosquitoAnt.





















