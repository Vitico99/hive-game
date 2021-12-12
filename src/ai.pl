:-module(ai,[]).
:-use_module(board).
:-use_module(bugs).
:-use_module(library(clpfd)).
:-use_module(heuristics).

:-dynamic target/2.
:- dynamic calculated/3.

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



% ========================================= Alpha Beta Algorithm ==========================================================


flip(maximize, minimize).
flip(minimize, maximize).

alphaBeta(Color, Move, Val):-
    heuristics:getAlpha(Color, Alpha),
    alphaBeta(Color, 2, Alpha, 300, Move, Val, maximize).

alphaBeta(Color, Depth, Alpha, Beta, Move, Val, Option):-
    Depth > 0, !,
    board:currentColor(Color1),
    moves(Color1, Moves), % get all possible moves
    boundedBest(Color, Moves, Depth, Alpha, Beta, Move, Val, Option);  % get the best
    heuristics:eval(Color1, Val). % This board is a final board so just return the score
    
boundedBest(Color, [Move|Moves], Depth, Alpha, Beta, BestMove, BestVal, Option):-
    flip(Option, Option1),
    Depth1 is Depth - 1,
    % write_ln(Move),
    evalAlphaBeta(Color, Move, Depth1, Alpha, Beta, MoveVal, Option1),
    % write_ln(MoveVal),
    goodEnough(Color, Moves, Depth, Alpha, Beta, Move, MoveVal, BestMove, BestVal, Option). % check if Move is the best possible move

evalAlphaBeta(Color, Move, Depth, Alpha, Beta, Val, Option):- % a wrapper to save and restore the board state for the backtrack
    board:saveBoard(Bugs,Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug ),
    move(Move),
    alphaBeta(Color, Depth, Alpha, Beta, _, Val, Option),
    board:loadBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug).

goodEnough(_, [], _, _, _, Move, MoveVal, Move, MoveVal, _):- !. % There are no more moves on the list so this has to do

goodEnough(Color, _, _, Alpha, Beta, Move, MoveVal, Move, MoveVal, maximize):- MoveVal >= Beta, !.
goodEnough(Color, _, _, Alpha, Beta, Move, MoveVal, Move, MoveVal, minimize):- MoveVal =< Alpha, !.

goodEnough(Color, Moves, Depth, Alpha, Beta, Move, MoveVal, BestMove, BestVal, Option):- % Couldn't prune so we need to check the other moves
    updateBounds(Color, Alpha, Beta, MoveVal, NewAlpha, NewBeta, Option), % update alpha or beta 
    boundedBest(Color, Moves, Depth, NewAlpha, NewBeta, Move1, MoveVal1, Option), % Recursively calculate the best value for the rest of the list
    selectByRecursion(Color, Move, MoveVal, Move1, MoveVal1, BestMove, BestVal, Option).

updateBounds(Color, Alpha, Beta, MoveVal, MoveVal, Beta, maximize):- % Alpha = max(Alpha, MoveVal)
    MoveVal > Alpha, !.
updateBounds(Color, Alpha, Beta, MoveVal, Alpha, MoveVal, minimize):- % Beta = min(Beta, MoveVal)
    MoveVal < Beta, !.
updateBounds(_, Alpha, Beta, _, Alpha, Beta, _).


selectByRecursion(Color, Move1, Val1, Move2, Val2, Move1, Val1, minimize):- Val1 < Val2, !.
selectByRecursion(Color, Move1, Val1, Move2, Val2, Move2, Val2, minimize):- Val1 > Val2, !.

selectByRecursion(Color, Move1, Val1, Move2, Val2, Move1, Val1, maximize):- Val1 > Val2, !.
selectByRecursion(Color, Move1, Val1, Move2, Val2, Move2, Val2, maximize):- Val1 < Val2, !.

selectByRecursion(Color, Move1, Val1, Move2, Val2, BestMove, BestVal, Option):-
    evalHeuristic(Color, Move1, HVal1),
    evalHeuristic(Color, Move2, HVal2),
    selectByHeuristic(Move1, HVal1, Move2, HVal2, BestMove, BestVal, Option).

selectByHeuristic(Move1, Val1, Move2, Val2, Move1, Val1, minimize):- Val1 < Val2, !.
selectByHeuristic(Move1, Val1, Move2, Val2, Move2, Val2, minimize):- Val1 >= Val2, !.

selectByHeuristic(Move1, Val1, Move2, Val2, Move1, Val1, maximize):- Val1 > Val2, !.
selectByHeuristic(Move1, Val1, Move2, Val2, Move2, Val2, maximize):- Val1 =< Val2, !.


evalHeuristic(Color, Move, Val):-
    board:saveBoard(Bugs,Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug ),
    move(Move),
    heuristics:eval(Color, Val),
    board:loadBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug).







