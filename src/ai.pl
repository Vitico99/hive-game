:-module(ai,[]).
:-use_module(board).
:-use_module(bugs).
:-use_module(library(clpfd)).
:-use_module(heuristics).

:-dynamic target/2.
:- dynamic calculated/3.

% ================================= Metrics ==========================================  

queenSurrounded(Color, 0):- \+ board:bug(Color, queen, _,_,_).
queenSurrounded(Color, Count):-
    board:bug(Color, queen, X1,Y1,0),
    !,
    aggregate_all(count, board:nonEmptyAdyacent(X1,Y1,X2,Y2), Count).

piecesPinned(Color, Count):-
    aggregate_all(count, pinnedBug(C,X,Y), Count).

pinnedBug(C,X,Y):-
    board:getCellTop(X,Y,S1),
    board:bug(C,_,X,Y,S2),
    S2 < S1.
pinnedBug(C,X,Y):-
    board:getCellTop(X,Y,S),
    (\+board:canBeRemoved(X,Y);
    \+board:canBeMoved(X,Y,S)).



adyacent(X,Y,[X1,Y1]):-
    board:adyacent(X,Y,X1,Y1).

bugsThreatQueen(Color, 0):- \+ board:bug(Color, queen, _,_,_).
bugsThreatQueen(Color, Count):-
    board:opponent(Color, OColor),
    findall([X1, Y1], possibleMove(OColor, [T,_, _, X1, Y1]), D1), % the queen can be only threat by moving pieces 
    board:bug(Color,queen,X,Y,_),
    include(adyacent(X,Y), D1, D2),
    sort(D2,D3), % remove possible duplicates
    length(D3, Count).


playedBugs(Color, Count):-
    aggregate_all(count, board:bug(Color,_,_,_,_), Count).
playedBugs(Color, 0).
% ================================= Eval Function==========================================  

metric_weight(queenSurrounded,1500).
metric_weight(queenThreats,300).
metric_weight(piecesPinned,200).
metric_weight(freeBugs, 100).
metric_weight(piecesMoves, 1).

eval(Color, 20000):- board:colorWin(Color), !.
eval(Color, -20000):- board:opponent(Color, C1), board:colorWin(C1),!.

eval(Color, 20000):-
    board:opponent(Color, OColor),
    queenSurrounded(OColor, 6).
eval(Color, Score):-
    board:opponent(Color, Ecolor),

    queenSurrounded(Color, QsC),
    queenSurrounded(Ecolor, QsE),
    QsMetric is 2 * QsE - QsC,

    %bugsThreatQueen(EColor, BtQO),
    %BtQMetric is BtQO,

    piecesPinned(Color, PpC),
    playedBugs(Color, PlBC),
    FbMetric is PlBC - PpC, 


    metric_weight(queenSurrounded, QS),
    metric_weight(piecesPinned, PP),
    metric_weight(queenThreats, QT),
    metric_weight(freeBugs, FB),
    %metric_weight(piecesMoves, PM),
    %scalar_product([QS,PP,PM],[QsMetric, PpMetric, PmMetric],#=,Score),!.
    %scalar_product([QS,QT,FB],[QsMetric,BtQMetric,FbMetric],#=,Score),!.
    scalar_product([QS,FB],[QsMetric,FbMetric],#=,Score),!.
    


% ============================================================================

moves(Color, Moves):-
    retractall(calculated(_,_,_)),
    findall(Move, possibleMove(Color, Move), Moves1),
    sort(Moves1, Moves2),
    heuristics:getOrderingHeuristic(Color, Ordering),
    predsort(Ordering, Moves2, Moves).

possibleMove(Color, [T, X1, Y1, X2, Y2]):-
    board:bug(Color, T, X1,Y1,S),
    board:canBeRemoved(X1,Y1),
    board:getCellTop(X1,Y1,S),
    board:canBeMoved(X1,Y1, S),
    (calculated(T,X1,Y1); bugs:getDestinations(X1,Y1,T, Color), assertz(calculated(C1,Y1,T))),
    bugs:destination(X2,Y2).

possibleMove(Color, [T,X,Y]):-
    board:placeableByColor(Color, T),
    board:placeableByColor(X, Y, Color).



move([T, X, Y]):-
    board:currentColor(C),
    board:placeBug(C, T, X, Y),
    board:updateBugCount(C, T),
    board:changeCurrentColor.
move([T, X1, Y1, X2, Y2]):-
    board:getCellTop(X1, Y1, S),
    board:bug(C, T, X1, Y1, S),
    board:removeBug(X1, Y1),
    board:placeBug(C, T, X2, Y2),
    board:changeCurrentColor.

retractMove([T,X,Y]):-
    removeBug(X,Y).
retractMove([T,X1,Y1,X2,Y2]):-
    removeBug(X2,Y2),
    board:currentColor(C),
    placeBug(C,T,X1,Y1).


% ========================================= Alpha Beta Algorithm ==========================================================


flip(maximize, minimize).
flip(minimize, maximize).

alphaBeta(Color, Move, Val):-
    alphaBeta(Color, 2, 0, 10000, Move, Val, maximize).

% alphaBeta(Color, 0, Alpha, Beta, Move, Val, Option):-
%     heuristics:eval(Color, Val).
    %eval(Color, Val).
alphaBeta(Color, Depth, Alpha, Beta, Move, Val, Option):-
    Depth > 0, !,
    board:currentColor(Color1),
    moves(Color1, Moves), % get all possible moves
    boundedBest(Color, Moves, Depth, Alpha, Beta, Move, Val, Option);  % get the best
    %eval(Color, Val).
    heuristics:eval(Color1, Val). % This board is a final board so just return the score

boundedBest(Color, [Move|Moves], Depth, Alpha, Beta, BestMove, BestVal, Option):-
    % play the move
    board:saveBoard(Bugs,Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug ),
    move(Move),
    write_ln(Move),
    flip(Option, Option1),
    Depth1 is Depth - 1,
    write_ln(Depth1),
    alphaBeta(Color, Depth1, Alpha, Beta, _, MoveVal, Option1), % get the alpha-beta approx value of Move
    write_ln(MoveVal),
    board:loadBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug),
    goodEnough(Color, Moves, Depth, Alpha, Beta, Move, MoveVal, BestMove, BestVal, Option). % check if Move is the best possible move

goodEnough(_, [], _, _, _, Move, MoveVal, Move, MoveVal, _):- !. % There are no more moves on the list so this has to do

goodEnough(Color, _, _, Alpha, Beta, Move, MoveVal, Move, MoveVal, maximize):- MoveVal >= Beta, !.
goodEnough(Color, _, _, Alpha, Beta, Move, MoveVal, Move, MoveVal, minimize):- MoveVal =< Alpha, !.

goodEnough(Color, Moves, Depth, Alpha, Beta, Move, MoveVal, BestMove, BestVal, Option):- % Couldn't prune so we need to check the other moves
    updateBounds(Color, Alpha, Beta, MoveVal, NewAlpha, NewBeta, Option), % update alpha or beta 
    boundedBest(Color, Moves, Depth, NewAlpha, NewBeta, Move1, MoveVal1, Option), % Recursively calculate the best value for the rest of the list
    selectByCrit(MoveVal, Move, MoveVal1, Move1, BestVal, BestMove, Option).

updateBounds(Color, Alpha, Beta, MoveVal, MoveVal, Beta, maximize):- % Alpha = max(Alpha, MoveVal)
    MoveVal > Alpha, !.
updateBounds(Color, Alpha, Beta, MoveVal, Alpha, MoveVal, minimize):- % Beta = min(Beta, MoveVal)
    MoveVal < Beta, !.
updateBounds(_, Alpha, Beta, _, Alpha, Beta, _).


%Comparing type of moves : [T,X,Y] <=> [T,X,Y]
selectByCrit(S1, [T1,X1,Y1], S2, [_,_,_], S1, [T1,X1,Y1], minimize):- S1 < S2, !.
selectByCrit(S1, [_,_,_], S2, [T2,X2,Y2], S2, [T2,X2,Y2], minimize):- S1 >= S2, !.
selectByCrit(S1, [T1,X1,Y1], S2, [_,_,_], S1, [T1,X1,Y1],maximize):- S1 > S2, !.
selectByCrit(S1, [_,_,_], S2, [T2,X2,Y2], S2, [T2,X2,Y2],maximize):- S1 =< S2, !.

%Comparing type of moves : [T,X1,Y1, X2, Y2] <=> [T,X1,Y1,X2,Y2]
selectByCrit(S1, [T1, X11, Y11, X12, Y12], S2, [_,_,_,_,_], S1, [T1, X11, Y11, X12, Y12], minimize):- S1 < S2,!.
selectByCrit(S1, [_,_,_,_,_], S2, [T2, X21, Y21, X22, Y22], S2, [T2, X21, Y21, X22, Y22],minimize):- S1 >= S2,!.
selectByCrit(S1, [T1, X11, Y11, X12, Y12], S2, [_,_,_,_,_], S1, [T1, X11, Y11, X12, Y12],maximize):- S1 > S2,!.
selectByCrit(S1, [_,_,_,_,_], S2, [T2, X21, Y21, X22, Y22], S2, [T2, X21, Y21, X22, Y22],maximize):- S1 =<S2,!.

%Comparing type of moves :  [T,X,Y] <=>[T,X1,Y1, X2, Y2]
selectByCrit(S1, [T,X,Y], S2, [_,_,_,_,_], S1, [T,X,Y], minimize ):- S1 < S2,!.
selectByCrit(S1, [_,_,_], S2, [T1, X1,Y1, X2,Y2], S2, [T1,X1,Y1,X2,Y2], minimize):- S1 >= S2,!.
selectByCrit(S1, [T,X,Y], S2, [_,_,_,_,_], S1, [T,X,Y], maximize):- S1 > S2,!.
selectByCrit(S1, [_,_,_], S2, [T1, X1,Y1, X2,Y2], S2, [T1,X1,Y1,X2,Y2],maximize):- S1 =<S2,!.

%Comparing type of moves :   [T,X1,Y1, X2, Y2]<=> [T,X,Y]
selectByCrit(S1, [T1, X1, Y1, X2, Y2], S2, [_,_,_], S1, [T1,X1,Y1,X2,Y2], minimize):- S1 =< S2, !.
selectByCrit(S1, [_,_,_,_,_], S2, [T,X,Y], S2, [T,X,Y], minimize):- S1 > S2, !.
selectByCrit(S1, [T1, X1, Y1, X2, Y2], S2, [_,_,_], S1, [T1,X1,Y1,X2,Y2],maximize):- S1 >= S2, !.
selectByCrit(S1, [_,_,_,_,_], S2, [T,X,Y], S2, [T,X,Y],maximize):- S1 < S2, !.


% ================================================= Helper ==================================







