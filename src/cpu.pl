:-module(cpu,[]).
:-use_module(board).
:-use_module(bugs).
:-use_module(library(clpfd)).

% ================================= Metrics ==========================================  

queenSurrounded(Color, 0):- \+ board:bug(Color, queen, _,_,_).
queenSurrounded(Color, Count):-
    board:bug(Color, queen, X1,Y1,0),
    !,
    findall([X2,Y2],board:nonEmptyAdyacent(X1,Y1,X2,Y2), Adys),
    length(Adys, Count).

piecesPinned(Color, Count):-
    findall(board:bug(Color, T, X,Y,S), board:bug(Color, T, X,Y,S), Bugs),
    countPinnedBugs(Bugs, Count).

countPinnedBugs([],0).
countPinnedBugs([board:bug(_,_,X,Y,S)|R], Count):-
    (\+board:canBeRemoved(X,Y);
    \+board:canBeMoved(X,Y,S)),
    !,
    countPinnedBugs(R,C1), Count is C1 + 1.

countPinnedBugs([board:bug(_,T,X,Y,_)|R], Count):-
    bugs:countDestinations(X,Y,T,Cp),
    Cp >0,
    !,
    countPinnedBugs(R,Count).

countPinnedBugs([board:bug(_,_,_,_,_)|R], Count):-
    countPinnedBugs(R,C1),
    !,
    Count is C1 +1.

piecesMoves(Color, Count):-
    findall(board:bug(Color, T, X,Y,S), board:bug(Color, T, X,Y,S), Bugs),
    countMoves(Bugs, Count).

countMoves([],0).
countMoves([board:bug(_,_,X,Y,S)|R], Count):-
    (\+board:canBeRemoved(X,Y);
    \+board:canBeMoved(X,Y,S)),
    !,
    countMoves(R,Count).
countMoves([board:bug(_,T,X,Y,_)|R], Count):-
    bugs:countDestinations(X,Y,T,Cm),
    !,
    countMoves(R,Cr),
    Count is Cm + Cr.

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
    scalar_product([QS,PP,PM],[QsMetric, PpMetric, PmMetric],#=,Score),!.

% =================================Minimax========================================== 

getPlaceMoves(Color,Moves):-
   findall([T,X,Y], placeMove(Color,X,Y,T),Moves1), 
   sort(Moves1,Moves).
   
getBoardMoves(Color, Moves):-
    findall([T,X1,Y1, X2, Y2],  boardMove(Color, X1,Y1, X2,Y2,T), Moves1),
    sort(Moves1,Moves).

placeMove(Color, X, Y, Type):-
    board:placeableByColor(X,Y,Color),
    board:placeableByColor(Color,Type).

boardMove(Color, X1,Y1, X2, Y2, T):-
    board:bug(Color, T, X1,Y1,_),
    board:canBeRemoved(X1,Y1),
    board:getCellTop(X1,Y1,S),
    board:canBeMoved(X1,Y1, S),
    board:bug(Color, T, X1,Y1,S),
    bugs:getDestinations(X1,Y1,T, Color),
    bugs:destination(X2,Y2).


flip(maximize, minimize).
flip(minimize, maximize).

% minimize(Color, Score, Move):-
%     board:opponent(Color,C),
%     getPlaceMoves(C, PlaceMoves),
%     getBoardMoves(C, BoardMoves),
%     minimizePlaceMove(Color, PlaceMoves, Score1, Move1),!,
%     minimizeBoardMove(Color, BoardMoves, Score2, Move2),!,
%     selectBest(Score1, Move1, Score2, Move2, Score, Move).

minimax(Color, 0, _, Score, [-1,-1,-1]):- 
    (eval(Color, Score); 
    write_ln("************************************"),
    findall(board:bug(C,T,X,Y,S), board:bug(C,T,X,Y,S), Bugs),
    write_ln(Bugs),
    write_ln("************************************")
    ),!. 
minimax(Color,Depth, Crit, Score, Move ):-
    Depth >0,
    write('Minimax: '), write_ln([Color, Depth, Crit]),
    
    
    getPlaceMoves(Color, PlaceMoves),
    getBoardMoves(Color, BoardMoves),
    minimaxPlaceMove(Color, PlaceMoves, Depth, Crit, Score1, Move1),!,
    minimaxBoardMove(Color, BoardMoves, Depth, Crit, Score2, Move2),!,
    selectBestByCrit(Score1, Move1, Score2, Move2, Score, Move,Crit).



minimaxPlaceMove(_, [],_, maximize, -1000000,[-1,-1,-1]):- !, true.
minimaxPlaceMove(_, [],_,minimize, 1000000,[-1,-1,-1]):- !,true.
minimaxPlaceMove(Color, [[T,X,Y]| R],Depth ,Crit, Score, Move):-
    board:saveBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug ),
    board:placeBug(Color, T, X, Y),
    board:updateBugCount(Color,T),
    board:changeCurrentColor,
    board:opponent(Color, Color1),
    flip(Crit, Crit1),
    Depth1 is Depth -1,
    minimax(Color1,Depth1, Crit1, Score1,_),!,
    board:loadBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug),
    minimaxPlaceMove(Color, R,Depth, Crit, Score2, Move2),!,
    selectByCrit(Score1,[T,X,Y], Score2, Move2, Score, Move, Crit).

% minimizePlaceMove(_,[], 1000000, [-1,-1,-1]):- !, true.
% minimizePlaceMove(Color, [[T,X,Y]|R],Score, Move):-
%     board:saveBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug ),
%     %Try the current move
%     board:placeBug(CurrentColor, T, X,Y),
%     board:updateBugCount(CurrentColor,T),
%     eval(Color, CurrScore),
%     board:loadBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug),
%     minimizePlaceMove(Color, R, Score1, Move1),!,
%     selectTheMin(CurrScore, [T,X,Y],Score1, Move1, Score, Move).


minimaxBoardMove(_, [],_, maximize, -1000000,[-1,-1,-1, -1, -1]):- !, true.
minimaxBoardMove(_, [],_,minimize, 1000000,[-1,-1,-1, -1, -1]):- !, true.
minimaxBoardMove(Color, [[T,X1,Y1,X2,Y2]|R], Depth, Crit, Score, Move):-
    board:saveBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug ),
    board:removeBug(X1,Y1),
    board:placeBug(Color, T, X2, Y2),
    board:opponent(Color, Color1),
    board:changeCurrentColor,
    flip(Crit, Crit1),
    Depth1 is Depth -1,
    minimax(Color1,Depth1, Crit1, Score1,_),!,
    board:loadBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug),
    minimaxBoardMove(Color,R, Depth, Crit, Score2, Move2),!,
    selectByCrit(Score1,[T,X1,Y1,X2,Y2], Score2, Move2, Score, Move, Crit).
 

% minimizeBoardMove(_,[], 1000000, [-1,-1,-1,-1,-1]):- !, true.
% minimizeBoardMove(Color, [[T,X1,Y1, X2, Y2]|R], Score,Move):-
%     board:saveBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug ),
%     board:removeBug(X1,Y1),
%     board:placeBug(CurrentColor, T, X2, Y2),
%     eval(Color, CurrScore),
%     board:loadBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug),
%     minimizeBoardMove(Color, R, Score1, Move1),!,
%     selectTheMin(CurrScore, [T,X1,Y1, X2, Y2], Score1, Move1, Score, Move).

%Comparing type of moves : [T,X,Y] <=> [T,X,Y]
selectByCrit(S1, [T1,X1,Y1], S2, [_,_,_], S1, [T1,X1,Y1], minimize):- S1 < S2, !.
selectByCrit(S1, [_,_,_], S2, [T2,X2,Y2], S2, [T2,X2,Y2], minimize):- S1 >= S2, !.
selectByCrit(S1, [T1,X1,Y1], S2, [_,_,_], S1, [T1,X1,Y1],maximize):- S1 > S2, !.
selectByCrit(S1, [_,_,_], S2, [T2,X2,Y2], S2, [T2,X2,Y2],maximize):- S1 =< S2, !.


%Comparing type of moves : [T,X1,Y1, X2, Y2] <=> [T,X1,Y1,X2,Y2]
selectByCrit(S1, [T1, X11, Y11, X12, Y12], S2, [_,_,_,_,_], S1,[T1, X11, Y11, X12, Y12], minimize):-
    S1 < S2,!.
selectByCrit(S1, [_,_,_,_,_], S2, [T2, X21, X22, Y21, Y22], S2,[T2, X21, Y21, X22, Y22],minimize):-
    S1 >= S2,!.
selectByCrit(S1, [T1, X11, Y11, X12, Y12], S2, [_,_,_,_,_], S1,[T1, X11, Y11, X12, Y12],maximize):-
    S1 > S2,!.
selectByCrit(S1, [_,_,_,_,_], S2, [T2, X21, X22, Y21, Y22], S2,[T2, X21, Y21, X22, Y22],maximize):-
    S1 =<S2,!.

%Comparing type of moves :  [T,X,Y] <=>[T,X1,Y1, X2, Y2]
selectBestByCrit(S1, [T,X,Y], S2, [_,_,_,_,_], S1, [T,X,Y], minimize ):- S1 < S2,!.
selectBestByCrit(S1, [_,_,_], S2, [T1, X1,Y1, X2,Y2], S2, [T1,X1,Y1,X2,Y2], minimize):- S1 >= S2,!.
selectBestByCrit(S1, [T,X,Y], S2, [_,_,_,_,_], S1, [T,X,Y], maximize):- S1 > S2,!.
selectBestByCrit(S1, [_,_,_], S2, [T1, X1,Y1, X2,Y2], S2, [T1,X1,Y1,X2,Y2],maximize):- S1 =<S2,!.


%Comparing type of moves :   [T,X1,Y1, X2, Y2<=> [T,X,Y]
selectBestByCrit(S1, [T1, X1, Y1, X2, Y2], S2, [_,_,_], S1, [T1,X1,Y1,X2,Y2], minimize):- S1 < S2, !.
selectBestByCrit(S1, [_,_,_,_,_], S2, [T,X,Y], S2, [T,X,Y], minimize):- S1 >= S2, !.
selectBestByCrit(S1, [T1, X1, Y1, X2, Y2], S2, [_,_,_], S1, [T1,X1,Y1,X2,Y2],maximize):- S1 > S2, !.
selectBestByCrit(S1, [_,_,_,_,_], S2, [T,X,Y], S2, [T,X,Y],maximize):- S1 =<S2, !.
