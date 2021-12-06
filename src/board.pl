:- module(board, []).

:- dynamic bug/5. % bug(Color, Type of Bug, X pos, Y pos, Stack pos)
:- dynamic frontier/2. % frontier(X,Y): cell(X,Y) is a frontier cell, this is an empty cell that is adyacent to a bug of the hive.
:- dynamic visited/2. % visited(X,Y): cell(X,Y) has been visited by dfs.

init_board():-
    assert(frontier(100,100)).

% Adyacent definition for an hexagonal grid
adyacent(X1,Y1,X2,Y2):- X2 is X1 - 1, Y2 is Y1.
adyacent(X1,Y1,X2,Y2):- X2 is X1 -1 , Y2 is Y1 + 1.
adyacent(X1,Y1,X2,Y2):- X2 is X1, Y2 is Y1 - 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1, Y2 is Y1 + 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1 + 1, Y2 is Y1 - 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1 + 1, Y2 is Y1.

empty(X,Y):- \+ (bug(_,_,X,Y,_)).

emptyAdyacent(X1, Y1, X2, Y2):-
    adyacent(X1, Y1, X2, Y2), 
    empty(X2, Y2).

nonEmptyAdyacent(X,Y, X1, Y1):-
    adyacent(X,Y,X1,Y1), 
    \+empty(X1,Y1).

frontierAdyacent(X1, Y1, X2, Y2):-
    adyacent(X1, Y1, X2, Y2),
    frontier(X2, Y2).

adyacentOpponent(X1, Y1, X2, Y2, C1):-
    adyacent(X1, Y1, X2, Y2),
    bug(C2, _, X2, Y2, _),
    C1 \== C2.

isIsolated(X,Y):-
    findall([X1,Y1], nonEmptyAdyacent(X,Y, X1,Y1),L),
    length(L, 1).
    
isolatedEmptyAdyacent(X1, Y1, X2, Y2):- % X2, Y2 is only adyacnet to X1,Y1
    emptyAdyacent(X1,Y1,X2,Y2),
    isIsolated(X2,Y2).

placeBug(C,T,X,Y):-
    assert(bug(C,T,X,Y,0)), retractall(frontier(X,Y)), 
    forall(emptyAdyacent(X,Y,X1,Y1), assert(frontier(X1, Y1))). % expand the frontier of the hive

getAllPlaceableCells(PlaceablePositions):-
    findall([X,Y],placeable(X,Y),PlaceablePositions).

% Checks if a bug of Color C can be placed at cell (X,Y)
placeableByColor(X,Y,C):-
    frontier(X,Y), 
    \+ adyacentOpponent(X,Y,_,_,C).

getBug(X,Y, S, bug(P,T,X,Y,S)):-%Get the bug in Position X,Y with stack number S
    bug(P,T,X,Y,S).

removeBug(X,Y):- % Remove Position X,Y. Assumes there is only one bug in cell.
    getBug(X,Y,0,Bug),
    forall(isolatedEmptyAdyacent(X,Y,X1,Y1), retract(placeable(X1,Y1))),
    retract(Bug),
    assert(placeable(X,Y)).


isBoardConnected():- \+bug(_,_,_,_,_), !.
isBoardConnected():-
    bug(_,_,X,Y,_),
    !,
    forall(visited(X1,Y1), retract(visited(X1,Y1))),
    isBoardConnected(X,Y).
isBoardConnected(X,Y):-
    assert(visited(X,Y)),
    forall(toVisit(X,Y,X2,Y2), isBoardConnected(X2,Y2)),
    findall([X3,Y3], visited(X3,Y3), AllVisited),
    findall([X4,Y4], bug(_,_,X4,Y4,0), NonEmptyCells ),
    same_length(AllVisited, NonEmptyCells).
    
toVisit(X,Y, X1,Y1):-
    nonEmptyAdyacent(X,Y,X1,Y1),
    \+visited(X1,Y1).