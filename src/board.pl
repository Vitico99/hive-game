:- module(board, []).

% Board structs
:- dynamic bug/5. % bug(Color, Type of Bug, X pos, Y pos, Stack pos)
:- dynamic frontier/2. % frontier(X,Y): cell(X,Y) is a frontier cell, this is an empty cell that is adyacent to a bug of the hive.

% Board state variables
:- dynamic color/1.
:- dynamic currentColor/1. 
:- dynamic availableBug/3. %availableBug(C, T, Cnt) there is Cnt bugs of type T and color C that can be placed

% Utils
:- dynamic visited/2. % visited(X,Y): cell(X,Y) has been visited by dfs.


% Adyacent definition for an hexagonal grid
adyacent(X1,Y1,X2,Y2):- X2 is X1 - 1, Y2 is Y1.
adyacent(X1,Y1,X2,Y2):- X2 is X1 -1 , Y2 is Y1 + 1.
adyacent(X1,Y1,X2,Y2):- X2 is X1, Y2 is Y1 - 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1, Y2 is Y1 + 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1 + 1, Y2 is Y1 - 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1 + 1, Y2 is Y1.

empty(X,Y):- \+ (bug(_,_,X,Y,_)). %ok

emptyAdyacent(X1, Y1, X2, Y2):- %ok
    adyacent(X1, Y1, X2, Y2), 
    empty(X2, Y2).

nonEmptyAdyacent(X,Y, X1, Y1):- %ok
    adyacent(X,Y,X1,Y1), 
    \+empty(X1,Y1).

frontierAdyacent(X1, Y1, X2, Y2):-
    adyacent(X1, Y1, X2, Y2),
    frontier(X2, Y2).

adyacentOpponent(X1, Y1, X2, Y2, C1):- %ok
    adyacent(X1, Y1, X2, Y2),
    bug(C2, _, X2, Y2, _),
    C1 \== C2.

% nonIsolated(X1,Y1,X2,Y2):-
%     nonEmptyAdyacent(X2, Y2, X3, Y3),
%     X1 \== X3,
%     Y1 \== Y3.

% isolated(X1,Y1,X2,Y2):-
%     \+ nonIsolated(X1,Y1,X2,Y2).


isIsolated(X,Y):-
    findall([X1,Y1], nonEmptyAdyacent(X,Y, X1,Y1),L),
    length(L, 1).
    
isolatedEmptyAdyacent(X1, Y1, X2, Y2):- % X2, Y2 is only adyacnet to X1,Y1
    emptyAdyacent(X1,Y1,X2,Y2),
    isIsolated(X2,Y2).

getAllPlaceableCells(PlaceablePositions):-
    findall([X,Y],placeable(X,Y),PlaceablePositions).

% Checks if a bug of Color C can be placed at cell (X,Y)
placeableByColor(X,Y,C):- %ok
    frontier(X,Y), 
    \+ adyacentOpponent(X,Y,_,_,C).

placeableByColor(X,Y,C):- % Edge case of the first bug of the second player
    firstBug(C),
    frontier(X,Y).

% Checks if a bug at cell (X,Y) can be removed from the hive
% A bug can be removed if and only if the hive remains connected without the bug.
canBeRemoved(X,Y):-
    nonEmptyAdyacent(X,Y,X2,Y2), assertz(visited(X,Y)),
    isBoardConnected(X2, Y2),
    findall([X3, Y3], visited(X3,Y3), AllVisited),
    findall([X4, Y4], bug(_,_,X4,Y4,0), NonEmptyCells),!,
    (\+forall(visited(X1,Y1), retract(visited(X1,Y1)));
    same_length(AllVisited,NonEmptyCells)).

isBoardConnected(X,Y):-
    assertz(visited(X,Y)),
    forall(toVisit(X,Y,X2,Y2), isBoardConnected(X2,Y2)).


getBug(X,Y, S, bug(P,T,X,Y,S)):-%Get the bug in Position X,Y with stack number S
    bug(P,T,X,Y,S).


toVisit(X,Y, X1,Y1):-
    nonEmptyAdyacent(X,Y,X1,Y1),
    \+visited(X1,Y1).




% =============================== Board state modifying predicates

initBoard(C1, C2):-
    assertz(frontier(100,100)),
    assertz(color(C1)), assertz(color(C2)),
    assertz(firstBug(C2)),
    assertz(currentColor(C1)),

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

changeCurrentColor:-
    %Add check if the non current player can move before updating
    color(C1), currentColor(C1),
    color(C2), \+ currentColor(C2),
    retract(currentColor(C1)), assertz(currentColor(C2)).

placeBug(C,T,X,Y):- %ok
    availableBug(C,T,Cnt), Cnt1 is Cnt - 1,
    retract(availableBug(C,T,Cnt)), assertz(availableBug(C,T,Cnt1)),
    checkFirstBug(C),
    assertz(bug(C,T,X,Y,0)), retractall(frontier(X,Y)), 
    forall(emptyAdyacent(X,Y,X1,Y1), assertz(frontier(X1, Y1))). % expand the frontier of the hive

checkFirstBug(C):-
    \+ firstBug(C).

checkFirstBug(C):-
    retract(firstBug(C)).
    
removeBug(X,Y):- % Remove Position X,Y. Assumes there is only one bug in cell.
    getBug(X,Y,0,Bug),
    forall(isolatedEmptyAdyacent(X,Y,X1,Y1), retractall(frontier(X1,Y1))),
    retract(Bug),
    assertz(frontier(X,Y)).