:-module(cpu,[]).
:-use_module(board).
:-use_module(bugs).
:-use_module(library(clpfd)).

% ================================= Metrics ==========================================  

queenSurrounded(Color, Count):-
    board:bug(Color, queen, X1,Y1,0),
    !,
    findall([X2,Y2],board:nonEmptyAdyacent(X1,Y1,X2,Y2), Adys),
    length(Adys, Count).

piecesPinned(Color, Count):-
    findall(board:bug(Color, T, X,Y,S), board:bug(Color, T, X,Y,S), Bugs),
    countPinnedBugs(Bugs, Count).

countPinnedBugs([],0).
countPinnedBugs([board:bug(_,T,X,Y,_)|R], Count):-
    board:canBeRemoved(X,Y),
    bugs:isPossibleDestination(X,Y, _, _,T),
    !,
    countPinnedBugs(R,Count).
countPinnedBugs([board:bug(_,T,X,Y,_)|R], Count):-
    (\+ board: canBeRemoved(X,Y);
    \+bugs:isPossibleDestination(X,Y, _, _,T)),
    !,
    countPinnedBugs(R,C1),
    Count is C1 +1.

piecesMoves(Color, Count):-
    findall(board:bug(Color, T, X,Y,S), board:bug(Color, T, X,Y,S), Bugs),
    countMoves(Bugs, Count).

countMoves([],0).
countMoves([board:bug(_,T,X,Y,_)|R], Count):-
    board:canBeRemoved(X,Y),
    !,
    findall([X1,Y1], bugs:isPossibleDestination(X,Y,X1,Y1,T),Moves),
    countMoves(R,C1),
    length(Moves,Cm),
    Count is Cm + C1.

countMoves([board:bug(_,_,X,Y,_)|R], Count):-
    \+ board:canBeRemoved(X,Y),
    !,
    countMoves(R,Count).

% ================================= Eval Function==========================================  

metric_weight(queenSurrounded,50).
metric_weight(piecesPinned,10).
metric_weight(piecesMoves, 1).

eval(Color, 10000):- board:colorWin(Color), !.
eval(Color, -10000):- board:opponent(Color, C1), board:colorWin(C1),!.

eval(Color, Score):-
    board:opponent(Color, Ecolor),

    queenSurrounded(Color, QsC),
    queenSurrounded(Ecolor, QsE),
    QsMetric is QsE - QsC,

    piecesPinned(Color, PpC),
    piecesPinned(Ecolor, PpE),
    PpMetric is PpE - PpC,
    
    piecesMoves(Color, PmC),
    piecesMoves(Ecolor, PmE),
    PmMetric is PmC - PmE,

    metric_weight(queenSurrounded, QS),
    metric_weight(piecesPinned, PP),
    metric_weight(piecesMoves, PM),
    scalar_product([QS,PP,PM],[QsMetric, PpMetric, PmMetric],#=,Score).

% =================================Minimax========================================== 

getPlaceMoves(Color,Moves):-
   findall([T,X,Y], placeMove(Color,X,Y,T),Moves1), 
   sort(Moves1,Moves).
   

placeMove(Color, X, Y, Type):-
    board:placeableByColor(X,Y,Color),
    board:placeableByColor(Color,Type).



