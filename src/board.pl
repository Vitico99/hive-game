:- module(board, []).

% Board structs
:- dynamic bug/5. % bug(Color, Type of Bug, X pos, Y pos, Stack pos)
:- dynamic frontier/2. % frontier(X,Y): cell(X,Y) is a frontier cell, this is an empty cell that is adyacent to a bug of the hive.

% Board state variables
:- dynamic color/1.
:- dynamic currentColor/1. 
:- dynamic currentTurn/2.
:- dynamic availableBug/3. %availableBug(C, T, Cnt) there is Cnt bugs of type T and color C that can be placed

:- dynamic visited/2. % visited(X,Y): cell(X,Y) has been visited by dfs.
:- dynamic lastPlacedBug/6. 
:- dynamic opponent/2.

opponent(white, black).
opponent(black, white).

% adyacent/4
% Adyacent definition for an hexagonal grid
adyacent(X1,Y1,X2,Y2):- X2 is X1 - 1, Y2 is Y1.
adyacent(X1,Y1,X2,Y2):- X2 is X1 -1 , Y2 is Y1 + 1.
adyacent(X1,Y1,X2,Y2):- X2 is X1, Y2 is Y1 - 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1, Y2 is Y1 + 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1 + 1, Y2 is Y1 - 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1 + 1, Y2 is Y1.

% empty/2
empty(X,Y):- \+ (bug(_,_,X,Y,_)). 

% emptyAdyacent/4
emptyAdyacent(X1, Y1, X2, Y2):- 
    adyacent(X1, Y1, X2, Y2), 
    empty(X2, Y2).

% nonEmptyAdyacent/4
nonEmptyAdyacent(X1, Y1, X2, Y2):-
    adyacent(X1, Y1, X2, Y2), 
    \+empty(X2, Y2).

% frontierAdyacent/4
frontierAdyacent(X1, Y1, X2, Y2):-
    adyacent(X1, Y1, X2, Y2),
    frontier(X2, Y2).

% adyacentOpponent/5
adyacentOpponent(X1, Y1, X2, Y2, C1):-
    adyacent(X1, Y1, X2, Y2),
    bug(C2, _, X2, Y2, _),
    C1 \== C2.

adyacentAlly(X1, Y1, X2, Y2, C1):-
    adyacent(X1, Y1, X2, Y2),
    bug(C2, _, X2, Y2, _),
    C1 == C2.



% isIsolated/2
% cell (X,Y) is isolated iff has only one non empty adyacent cell
isIsolated(X,Y):-
    findall([X1,Y1], nonEmptyAdyacent(X,Y, X1,Y1),L),
    length(L, 1).
    
% isolatedEmptyAdyacent/4
isolatedEmptyAdyacent(X1, Y1, X2, Y2):- % X2, Y2 is only adyacent to X1,Y1
    emptyAdyacent(X1,Y1,X2,Y2),
    isIsolated(X2,Y2).

commonAdyacent(X1,Y1,X2,Y2,X3,Y3):-
    adyacent(X1,Y1,X3,Y3),
    adyacent(X2,Y2,X3,Y3).

commonEmptyAdyacent(X1,Y1,X2,Y2,X3,Y3):-
    commonAdyacent(X1,Y1,X2,Y2,X3,Y3),
    empty(X3,Y3).
    

accesibleCell(X1,Y1,X2,Y2):-
    \+empty(X1,Y1),
    frontierAdyacent(X1,Y1,X2,Y2),
    aggregate_all(count, commonEmptyAdyacent(X1,Y1,X2,Y2,_,_), C1), C1 > 0,
    aggregate_all(count, nonEmptyAdyacent(X2,Y2,_,_), C2), C2 > 1.

accesibleCell(X1,Y1,X2,Y2):-
    empty(X1,Y1),
    frontierAdyacent(X1,Y1,X2,Y2),
    aggregate_all(count, commonEmptyAdyacent(X1,Y1,X2,Y2,_,_), C1), C1 > 0,
    aggregate_all(count, nonEmptyAdyacent(X2,Y2,_,_), C2), C2 > 0.

% cellNonStacked/2
% there are no stacked bugs at cell (X,Y)
cellNonStacked(X,Y):-
    getCellHeight(X,Y,H),
    H =< 1.

% The height of a cell is the number of bugs stacked in the cell
getCellHeight(X,Y,H):-
    aggregate_all(count, bug(_,_,X,Y,_), H).

getCellTop(X,Y,S):-
    getCellHeight(X,Y,H),
    S is H-1.

placeableByColor(C,queen):-
    availableBug(C,queen,Cnt),
    Cnt > 0.

placeableByColor(C,T):-
    bug(C,queen,_,_,_),
    availableBug(C,T,Cnt),
    Cnt > 0.

placeableByColor(C,T):-
    currentTurn(C,N),
    N < 4,
    availableBug(C,T,Cnt),
    Cnt > 0.

% placeableByColor/3
% Checks if a bug of Color C can be placed at cell (X,Y)
placeableByColor(X,Y,C):- %ok
    frontier(X,Y), 
    \+ adyacentOpponent(X,Y,_,_,C).

placeableByColor(X,Y,C):- % Edge case of the first bug of the second player
    firstBug(C),
    frontier(X,Y).


% canBeRemoved/2
% Checks if a bug at cell (X,Y) can be removed from the hive
% A bug can be removed if and only if the hive remains connected without the bug.
canBeRemoved(X, Y):-
    getCellHeight(X,Y,H), H > 1.
canBeRemoved(X, Y):-
    nonEmptyAdyacent(X,Y,X2,Y2), assertz(visited(X,Y)),
    visit(X2, Y2),
    findall([X3, Y3], visited(X3,Y3), AllVisited),
    findall([X4, Y4], bug(_,_,X4,Y4,0), NonEmptyCells),!,
    (\+forall(visited(X1,Y1), retract(visited(X1,Y1)));
    same_length(AllVisited,NonEmptyCells)).

% visit/2
visit(X, Y):-
    assertz(visited(X, Y)),
    forall(toVisit(X, Y,X2,Y2), visit(X2,Y2)).

% toVisit/2
toVisit(X,Y, X1,Y1):-
    nonEmptyAdyacent(X,Y,X1,Y1),
    \+visited(X1,Y1).

% =============================== Board state modifying predicates

initBoard(C1, C2):-
    assertz(frontier(10,10)),
    assertz(color(C1)), assertz(color(C2)),
    assertz(firstBug(C2)),
    assertz(currentColor(C1)),
    % assertz(opponent(C1,C2)),
    % assertz(opponent(C2,C1)),
    assertz(currentTurn(C1,1)),
    assertz(currentTurn(C2,1)),
    assertz(availableBug(C1, queen, 1)),
    assertz(availableBug(C1, beetle, 2)),
    assertz(availableBug(C1, grasshoper, 3)),
    assertz(availableBug(C1, spider, 2)),
    assertz(availableBug(C1, ant, 3)),
    assertz(availableBug(C1, ladybug, 1)),
    assertz(availableBug(C1, mosquito, 1)),
    assertz(availableBug(C1, pigbull, 1)),

    assertz(availableBug(C2, queen, 1)),
    assertz(availableBug(C2, beetle, 2)),
    assertz(availableBug(C2, grasshoper, 3)),
    assertz(availableBug(C2, spider, 2)),
    assertz(availableBug(C2, ant, 3)),
    assertz(availableBug(C2, ladybug, 1)),
    assertz(availableBug(C2, mosquito, 1)),
    assertz(availableBug(C2, pigbull, 1)).

% changeCurrentColor/0
changeCurrentColor:-
    %Add check if the non current player can move before updating
    color(C1), currentColor(C1),
    color(C2), \+ currentColor(C2),
    retract(currentColor(C1)), assertz(currentColor(C2)).

% placeBug/4
placeBug(C,T,X,Y):- 
    checkFirstBug(C),
    getCellHeight(X,Y,H),
    assertz(bug(C,T,X,Y,H)), unsetFrontier(X,Y),
    currentColor(C1),
    retractall(lastPlacedBug(C1,_,_,_,_,_)), assertz(lastPlacedBug(C1,C,T,X,Y,H)),
    updateCurrentTurn(C1),
    forall(emptyAdyacent(X,Y,X1,Y1), setFrontier(X1,Y1)). % expand the frontier of the hive

% removeBug/2
removeBug(X,Y):- % Remove Position X,Y. Assumes there is only one bug in cell.
    getCellTop(X,Y,S),
    S == 0,!,
    getBug(X,Y,S,Bug),
    forall(isolatedEmptyAdyacent(X,Y,X1,Y1), unsetFrontier(X1,Y1)),
    retract(Bug),
    setFrontier(X,Y).
removeBug(X,Y):-
    getCellTop(X,Y,S),
    S >=1,!,
    getBug(X,Y,S,Bug),
    retract(Bug).
removeBug(_,_).

% updateBugCount/2
updateBugCount(C,T):-
    availableBug(C,T,Cnt1), Cnt2 is Cnt1 - 1,
    retract(availableBug(C,T,Cnt1)), assertz(availableBug(C,T,Cnt2)).

% checkFirstBug/1
checkFirstBug(C):-
    \+ firstBug(C).

checkFirstBug(C):-
    retract(firstBug(C)).

updateCurrentTurn(C):-
    currentTurn(C,N),
    N1 is N + 1,
    retract(currentTurn(C,N)),
    assertz(currentTurn(C,N1)).

% ================================= Utills ==========================================    

% cellsAreDistinct/4
cellsAreDistinct(X1,_, X2,_):- X1\==X2, !.
cellsAreDistinct(_, Y1,_, Y2):- Y1 \== Y2 ,!.
 
% cellsAreDistinct/3
cellsAreDistinct(_,_,[]).
cellsAreDistinct(X1, Y1,[[X2,Y2]| R]):-
    cellsAreDistinct(X1,Y1, X2, Y2),
    cellsAreDistinct(X1,Y1, R).

% cellsAreDistinct/1
cellsAreDistinct([]).
cellsAreDistinct([[X1,Y1]| R]):-
    cellsAreDistinct(X1,Y1, R),
    cellsAreDistinct(R).

% getBug/4
% Get the bug in Position X,Y with stack number S
getBug(X, Y, S, bug(P,T,X,Y,S)):- 
    bug(P,T,X,Y,S).

canBeMoved(X, Y, S):- %TODO fix this to remove the connection test when moving stacked bugs
    bug(C,_,X,Y,S),
    bug(C,queen,_,_,_),
    \+ lastPlacedBug(_,_,_,X,Y,S).

canBeMoved(X, Y, S):-
    bug(C, _, X, Y, S),
    bug(C, queen, _, _, _),
    lastPlacedBug(C1, C2, _, X, Y, S),
    C1 == C2.

% opponent(C1, C2):-
%     color(C1),
%     color(C2),
%     C1 \== C2.

setFrontier(X,Y):-
    frontier(X,Y).

setFrontier(X,Y):-
    assertz(frontier(X,Y)).

saveBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug, FirstBug):-
    findall(bug(C1,T1,X1,Y1,S1), bug(C1,T1,X1,Y1,S1), Bugs),
    findall(frontier(X2,Y2), frontier(X2,Y2), Frontier),
    currentColor(CurrentColor),
    findall(currentTurn(X3,Y3), currentTurn(X3,Y3), CurrentTurn),
    findall(availableBug(C4,T4,Cnt4), availableBug(C4,T4,Cnt4), AvailableBugs),
    findall( lastPlacedBug(C51,C5, T5,X5,Y5,S5), lastPlacedBug(C51,C5,T5,X5,Y5,S5), LastPlacedBug),
    findall(firstBug(C6), firstBug(C6), FirstBug).




clearBoard():-
    retractall(bug(_,_,_,_,_)),
    retractall(frontier(_,_)),
    retractall(currentColor(_)),
    retractall(currentTurn(_,_)),
    retractall(availableBug(_,_,_)),
    retractall(lastPlacedBug(_,_,_,_,_,_)),
    retractall(firstBug(_)).

loadBoard(Bugs, Frontier, CurrentColor, CurrentTurn, AvailableBugs,LastPlacedBug,FirstBug):-
    clearBoard,
    assertzList(Bugs),
    assertzList(Frontier),
    assertz(currentColor(CurrentColor)),
    assertzList(CurrentTurn),
    assertzList(AvailableBugs),
    assertzList(LastPlacedBug),
    assertzList(FirstBug). 

    
assertzList([]).
assertzList([H|T]):-assertz(H), assertzList(T).
unsetFrontier(X,Y):-
    frontier(X,Y),
    retract(frontier(X,Y)).
unsetFrontier(_,_).

% ================================= Metrics ==========================================  
colorWin(C):-
    opponent(C, C1),
    bug(C1, queen, X1, Y1,0),!,
    \+ emptyAdyacent(X1,Y1,_,_). 



  
