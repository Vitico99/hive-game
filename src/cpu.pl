:-module(cpu,[]).
:-use_module(board).
:-use_module(bugs).
:-use_module(library(clpfd)).

:- dynamic calculated/3.

% ================================= Metrics ==========================================  

queenSurrounded(Color, 0):- \+ board:bug(Color, queen, _,_,_).
queenSurrounded(Color, Count):-
    board:bug(Color, queen, X1,Y1,0),
    !,
    aggregate_all(count, board:nonEmptyAdyacent(X1,Y1,X2,Y2), Count).

% piecesPinned(Color, Count):-
%     findall(board:bug(Color, T, X,Y,S), board:bug(Color, T, X,Y,S), Bugs),
%     countPinnedBugs(Bugs, Count).

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
    

% countPinnedBugs([],0).
% countPinnedBugs([board:bug(_,_,X,Y,S)|R], Count):-
%     (\+board:canBeRemoved(X,Y);
%     \+board:canBeMoved(X,Y,S)),
%     !,
%     countPinnedBugs(R,C1), Count is C1 + 1.

% countPinnedBugs([board:bug(_,T,X,Y,_)|R], Count):-
%     bugs:countDestinations(X,Y,T,Cp),
%     Cp >0,
%     !,
%     countPinnedBugs(R,Count).

% countPinnedBugs([board:bug(_,_,_,_,_)|R], Count):-
%     countPinnedBugs(R,C1),
%     !,
%     Count is C1 +1.

% piecesMoves(Color, Count):-
%     findall(board:bug(Color, T, X,Y,S), board:bug(Color, T, X,Y,S), Bugs),
%     countMoves(Bugs, Count).

% countMoves([],0).
% countMoves([board:bug(_,_,X,Y,S)|R], Count):-
%     (\+board:canBeRemoved(X,Y);
%     \+board:canBeMoved(X,Y,S)),
%     !,
%     countMoves(R,Count).
% countMoves([board:bug(_,T,X,Y,_)|R], Count):-
%     bugs:countDestinations(X,Y,T,Cm),
%     !,
%     countMoves(R,Cr),
%     Count is Cm + Cr.

piecesMoves(Color, Count):-
    moves(Color, Moves),
    length(Moves, Count).

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
    
    %piecesMoves(Color, PmC),
    %piecesMoves(Ecolor, PmE),
    %PmMetric is PmC - PmE,

    metric_weight(queenSurrounded, QS),
    metric_weight(piecesPinned, PP),
    %metric_weight(piecesMoves, PM),
    %scalar_product([QS,PP,PM],[QsMetric, PpMetric, PmMetric],#=,Score),!.
    scalar_product([QS,PP],[QsMetric, PpMetric],#=,Score),!.
    %Score is QS * QsMetric.
% =================================Minimax========================================== 

getPlaceMoves(Color,Moves):-
   findall([T,X,Y], placeMove(Color,X,Y,T),Moves1), 
   sort(Moves1,Moves).
   
getBoardMoves(Color, Moves):-
    retractall(calculated(_,_,_)),
    findall([T,X1,Y1, X2, Y2],  boardMove(Color, X1,Y1, X2,Y2,T), Moves1),
    sort(Moves1,Moves).

placeMove(Color, X, Y, Type):-
    board:placeableByColor(Color,Type),
    board:placeableByColor(X,Y,Color).

boardMove(Color, X1,Y1, X2, Y2, T):-
    board:bug(Color, T, X1,Y1,S),
    board:canBeRemoved(X1,Y1),
    board:getCellTop(X1,Y1,S),
    board:canBeMoved(X1,Y1, S),
    (calculated(T,X1,Y1); bugs:getDestinations(X1,Y1,T, Color), assertz(calculated(C1,Y1,T))),
    bugs:destination(X2,Y2).


flip(maximize, minimize).
flip(minimize, maximize).


minimax(Color, 0, _, Score, [-1,-1,-1]):- 
    (eval(Color, Score); 
    write_ln("************************************"),
    findall(board:bug(C,T,X,Y,S), board:bug(C,T,X,Y,S), Bugs), %Like throw exception
    write_ln(Bugs),
    write_ln("************************************")
    ),!. 
minimax(Color,Depth, Crit, Score, Move ):-
    Depth >0,
    getPlaceMoves(Color, PlaceMoves),
    getBoardMoves(Color, BoardMoves),
    % getFirsts(PlaceMoves1,3, PlaceMoves),   % Maybe Limit the numbers of plays ?
    % getFirsts(BoardMoves1,3,BoardMoves),
    selectInitialScoreAndMove(Crit,Acc, AccMove),
    minimaxPlaceMove(Color, PlaceMoves, Depth, Crit,Acc, AccMove, Score1, Move1),!,
    minimaxBoardMove(Color, BoardMoves, Depth, Crit,Acc, AccMove, Score2, Move2),!,
    selectByCrit(Score1, Move1, Score2, Move2, Score, Move,Crit).

selectInitialScoreAndMove(maximize, -100000, [-1,-1,-1]).
selectInitialScoreAndMove(minimize, 100000, [-1,-1,-1]).



minimaxPlaceMove(_, [], _, _, Acc, AccMove, Acc, AccMove):- !, true.
minimaxPlaceMove(Color, [[T,X,Y]| R],Depth ,Crit,Acc,AccMove, Score, Move):-
    board:saveBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug ),
    board:placeBug(Color, T, X, Y),
    board:updateBugCount(Color,T),
    board:changeCurrentColor,
    eval(Color, TempScore),!,
    selectCurrent(Color, Depth, Crit,Acc,AccMove, TempScore, [T,X,Y], NewAcc, NewMove),!,
    board:loadBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug),
    minimaxPlaceMove(Color, R,Depth, Crit,NewAcc,NewMove, Score, Move),!.
    % selectByCrit(Score1,[T,X,Y], Score2, Move2, Score, Move, Crit).

selectCurrent(_, _,maximize, Acc,AccMove, TempScore,_,Acc, AccMove):- TempScore =< Acc,!.
selectCurrent(_, _,minimize, Acc, AccMove, TempScore,_,Acc, AccMove):- TempScore >= Acc,!.
selectCurrent(Color, Depth,Crit,Acc,AccMove,_, CurrMove, NewAcc, NewMove):-
    board:opponent(Color, Color1),
    flip(Crit, Crit1),
    Depth1 is Depth -1,
    minimax(Color1,Depth1,Crit1, Score1,_),!,
    selectByCrit(Score1, CurrMove, Acc, AccMove, NewAcc, NewMove, Crit).

minimaxBoardMove(_,[],_,_,Acc, AccMove, Acc, AccMove):- !, true.
minimaxBoardMove(Color, [[T,X1,Y1,X2,Y2]|R], Depth, Crit,Acc, AccMove, Score, Move):-
    board:saveBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug ),
    board:removeBug(X1,Y1),
    board:placeBug(Color, T, X2, Y2),
    board:changeCurrentColor,
    eval(Color, TempScore),
    selectCurrent(Color, Depth, Crit, Acc, AccMove, TempScore,[T,X1,Y1,X2,Y2], NewAcc, NewMove),!,
    board:loadBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug),
    minimaxBoardMove(Color,R, Depth, Crit,NewAcc,NewMove, Score, Move),!.
 


%Comparing type of moves : [T,X,Y] <=> [T,X,Y]
selectByCrit(S1, [T1,X1,Y1], S2, [_,_,_], S1, [T1,X1,Y1], minimize):- S1 < S2, !.
selectByCrit(S1, [_,_,_], S2, [T2,X2,Y2], S2, [T2,X2,Y2], minimize):- S1 >= S2, !.
selectByCrit(S1, [T1,X1,Y1], S2, [_,_,_], S1, [T1,X1,Y1],maximize):- S1 > S2, !.
selectByCrit(S1, [_,_,_], S2, [T2,X2,Y2], S2, [T2,X2,Y2],maximize):- S1 =< S2, !.


%Comparing type of moves : [T,X1,Y1, X2, Y2] <=> [T,X1,Y1,X2,Y2]
selectByCrit(S1, [T1, X11, Y11, X12, Y12], S2, [_,_,_,_,_], S1, [T1, X11, Y11, X12, Y12], minimize):-
    S1 < S2,!.
selectByCrit(S1, [_,_,_,_,_], S2, [T2, X21, Y21, X22, Y22], S2, [T2, X21, Y21, X22, Y22],minimize):-
    S1 >= S2,!.
selectByCrit(S1, [T1, X11, Y11, X12, Y12], S2, [_,_,_,_,_], S1, [T1, X11, Y11, X12, Y12],maximize):-
    S1 > S2,!.
selectByCrit(S1, [_,_,_,_,_], S2, [T2, X21, Y21, X22, Y22], S2, [T2, X21, Y21, X22, Y22],maximize):-
    S1 =<S2,!.

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

% Get the first Count Elements of the List L
getFirsts(L, Count, L):- length(L, Len), Count>= Len,!.
getFirsts([X|R], 1, [X]):- !, true.
getFirsts([X|R], Count, [X|R1]):-
    Count1 is Count -1,
    getFirsts(R ,Count1, R1).
