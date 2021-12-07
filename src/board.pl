:- module(board, []).

% Board structs
:- dynamic bug/5. % bug(Color, Type of Bug, X pos, Y pos, Stack pos)
:- dynamic frontier/2. % frontier(X,Y): cell(X,Y) is a frontier cell, this is an empty cell that is adyacent to a bug of the hive.

% Board state variables
:- dynamic color/1.
:- dynamic currentColor/1. 
:- dynamic availableBug/3. %availableBug(C, T, Cnt) there is Cnt bugs of type T and color C that can be placed

:- dynamic visited/2. % visited(X,Y): cell(X,Y) has been visited by dfs.
:- dynamic lastPlacedBug/5. % lastPlacedBug(Bug, C)
:- dynamic opponent/2.

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

% isIsolated/2
% cell (X,Y) is isolated iff has only one non empty adyacent cell
isIsolated(X,Y):-
    findall([X1,Y1], nonEmptyAdyacent(X,Y, X1,Y1),L),
    length(L, 1).
    
% isolatedEmptyAdyacent/4
isolatedEmptyAdyacent(X1, Y1, X2, Y2):- % X2, Y2 is only adyacent to X1,Y1
    emptyAdyacent(X1,Y1,X2,Y2),
    isIsolated(X2,Y2).

% accesibleCell/4
accesibleCell(X1, Y1, X2, Y2):-
    frontierAdyacent(X1,Y1,X2,Y2),
    emptyAdyacent(X2,Y2, X3,Y3),
    emptyAdyacent(X1, Y1, X3,Y3),    
    nonEmptyAdyacent(X2,Y2, X4,Y4),
    cellsAreDistinct(X1,Y1, X4,Y4).

% cellNonStacked/2
% there are no stacked bugs at cell (X,Y)
cellNonStacked(X,Y):-
    findall(S, bug(_,_,X,Y,S), Stack),
    length(Stack, L),
    L =< 1.

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
    assertz(opponent(C1,C2)),
    assertz(opponent(C2,C1)),


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
    assertz(bug(C,T,X,Y,0)), retractall(frontier(X,Y)), 
    retractall(lastPlacedBug(C,_,_,_,_)), assertz(lastPlacedBug(C,T,X,Y,0)),
    forall(emptyAdyacent(X,Y,X1,Y1), assertz(frontier(X1, Y1))). % expand the frontier of the hive

% removeBug/2
removeBug(X,Y):- % Remove Position X,Y. Assumes there is only one bug in cell.
    getBug(X,Y,0,Bug),
    forall(isolatedEmptyAdyacent(X,Y,X1,Y1), retractall(frontier(X1,Y1))),
    retract(Bug),
    assertz(frontier(X,Y)).

% updateBugCount/2
updateBugCount(C,T):-
    availableBug(C,T,Cnt1), Cnt2 is Cnt1 - 1,
    retract(availableBug(C,T,Cnt1)), assertz(availableBug(C,T,Cnt2)).

% checkFirstBug/1
checkFirstBug(C):-
    \+ firstBug(C).

checkFirstBug(C):-
    retract(firstBug(C)).
    
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