:-module(cpu,[]).
:-use_module(board).
:-use_module(bugs).
:-use_module(library(clpfd)).

% ================================= Metrics ==========================================  

queenSurrounded(Color, 0):- \+ board:bug(Color, queen, _,_,_).
queenSurrounded(Color, Count):-
    board:bug(Color, queen, X1,Y1,0), !,
    aggregate_all(count, board:nonEmptyAdyacent(X1,Y1,X2,Y2), Count).


piecesPinned(Color, Count):-
    aggregate_all(count, pinnedBug(C,X,Y), Count).

pinnedBug(C,X,Y):-
    board:getCellTop(X,Y,S1),
    board:bug(C,_,X,Y,S2),
    S2 < S1.
pinnedBug(C,X,Y):-
    board:getCellTop(X,Y,S),
    (\+canBeRemoved(X,Y);
    \+canBeMoved(X,Y,S)).
    

piecesMoves(Color, Count):-
    moves(Color, Moves),
    length(Moves, length).
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

    % piecesPinned(Color, PpC),
    % piecesPinned(Ecolor, PpE),
    % PpMetric is PpE - PpC,
    
    % piecesMoves(Color, PmC),
    % piecesMoves(Ecolor, PmE),
    % PmMetric is PmC - PmE,

    metric_weight(queenSurrounded, QS),
    % metric_weight(piecesPinned, PP),
    % metric_weight(piecesMoves, PM),
    % scalar_product([QS,PP,PM],[QsMetric, PpMetric, PmMetric],#=,Score),!.
    % scalar_product([QS,PP],[QsMetric, PpMetric],#=,Score),!.
    Score is QS * QsMetric.




% ============================================================================

moves(Color, Moves):-
    findall(Move, possibleMove(Color, Move), Moves).

possibleMove(Color, [T,X,Y]):-
    board:placeableByColor(Color, T),
    board:placeableByColor(X, Y, Color).

possibleMove(Color, [T, X1, Y1, X2, Y2]):-
    board:bug(Color, T, X1,Y1,S),
    board:canBeRemoved(X1,Y1),
    board:getCellTop(X1,Y1,S),
    board:canBeMoved(X1,Y1, S),
    (calculated(T,X1,Y1); bugs:getDestinations(X1,Y1,T, Color), assertz(calculated(C1,Y1,T))),
    bugs:destination(X2,Y2).

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


% ========================================= Alpha Beta Algorithm ==========================================================

alphaBeta(Color, Move, Val):-
    assertz(target(Color, maximize),
    opponent(Color, OColor),
    assertz(target(OColor, minimize)),
    alphaBeta(Color, 4, -infinity, +infinity, Move, Val).

alphaBeta(Color, Depth, Alpha, Beta, Move, Val):- 
    Depth > 0,!,
    moves(Color, Moves),!, % get all possible moves
    boundedBest(Moves, Depth, Alpha, Beta, Move, Val); % get the best 
    eval(Color, Val). % This board is a final board so just return the score

boundedBest(Color, [Move|Moves], Depth, Alpha, Beta, BestMove, BestVal):-
    % play the move
    move(Move),
    opponent(Color, OColor),
    alphaBeta(OColor, Depth - 1, Alpha, Beta, _, MoveVal), % get the alpha-beta approx value of Move 
    goodEnough(Color, Moves, Depth, Alpha, Beta, Move, MoveVal, BestMove, BestVal). % check if Move is the best possible move

goodEnough(_, [], _, _, _, Move, MoveVal, Move, MoveVal):- !. % There are no more moves on the list so this has to do
goodEnough(Color, _, _, Alpha, Beta, Move, MoveVal, Move, MoveVal):- % Try to prune the tree using the alpha beta condition
    target(Color, maximize), MoveVal > Beta, !;
    target(Color, minimize), MoveVal < Alpha, !.
goodEnough(Color, Moves, Depth, Alpha, Beta, Move, MoveVal, BestMove, BestVal):- % Couldn't prune so we need to check the other moves
    updateBounds(Color, Alpha, Beta, MoveVal, NewAlpha, NewBeta), % update alpha or beta 
    boundedBest(Color, Moves, Depth, NewAlpha, NewBeta, Move1, MoveVal1), % Recursively calculate the best value for the rest of the list
    target(Color, Operation),
    selectByCrit(MoveVal, Move, MoveVal1, Move1, BestVal, BestMove, Operation).

updateBounds(Color, Alpha, Beta, MoveVal, MoveVal, Beta):- % Alpha = max(Alpha, MoveVal)
    target(Color, maximize), MoveVal > Alpha, !.
updateBounds(Color, Alpha, Beta, MoveVal, Alpha, Beta):- % Beta = min(Beta, MoveVal)
    target(Color, minimize), MoveVal < Beta, !.
updateBounds(_, Alpha, Beta, _, Alpha, Beta).


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
selectByCrit(S1, [T1, X1, Y1, X2, Y2], S2, [_,_,_], S1, [T1,X1,Y1,X2,Y2], minimize):- S1 < S2, !.
selectByCrit(S1, [_,_,_,_,_], S2, [T,X,Y], S2, [T,X,Y], minimize):- S1 >= S2, !.
selectByCrit(S1, [T1, X1, Y1, X2, Y2], S2, [_,_,_], S1, [T1,X1,Y1,X2,Y2],maximize):- S1 > S2, !.
selectByCrit(S1, [_,_,_,_,_], S2, [T,X,Y], S2, [T,X,Y],maximize):- S1 =<S2, !.


% ================================================= Helper ==================================







