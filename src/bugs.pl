:-module(bugs, []).
:-use_module(board).

:- dynamic isPossibleDestination/5.
:- dynamic getDestinations/3.

:- dynamic destination/2.
:- dynamic visited/2.


% ================================= API ====================================
% This section provides two predicates to obtain the possible moves for a bug:
% - isPossibleDestination/5: exposes a generator for possible moves according to the type of bug
% - getDestinations/3: precomputes the possible moves to be exposed throw isPossibleDestination.


% getDestinations/3
getDestinations(_,_,queen).
getDestinations(_,_,beetle).
getDestinations(X,Y,grasshoper):-
    grasshoperDestinations(X,Y).
getDestinations(_,_,spider).
getDestinations(X,Y,ant):-
    antDestinations(X,Y).
getDestinations(_,_,ladybug).


% isPossibleDestination/5
isPossibleDestination(X1, Y1, X2, Y2, queen):- % Queen
    board:accesibleCell(X1, Y1, X2, Y2).

isPossibleDestination(X1, Y1, X2, Y2, beetle):- % Beetle
    board:accesibleCell(X1,Y1,X2,Y2);
    board:nonEmptyAdyacent(X1,Y1,X2,Y2).

isPossibleDestination(_, _, X2, Y2, grasshoper):- % Grasshoper
    destination(X2,Y2).

isPossibleDestination(X1, Y1, X2, Y2, spider):- % Spider
    spiderDestination(X1, Y1, X2, Y2).

isPossibleDestination(_, _, X2, Y2, ant):- % Ant
    destination(X2,Y2).

isPossibleDestination(X1, Y1, X2, Y2, ladybug):- % Ladybug
    ladybugDestination(X1, Y1, X2, Y2).

% ============================== Implemenation =================================
% Implementation of more complex moves generators

% Grasshoper

grasshoperDestinations(X, Y):-
    forall(destination(A,B), retract(destination(A,B))),
    forall(board:nonEmptyAdyacent(X, Y, X1, Y1), grasshoperVisit(X, Y, X1, Y1)).

grasshoperVisit(_,_,X2,Y2):-
    board:empty(X2,Y2),
    !,
    assert(destination(X2,Y2)).
grasshoperVisit(X1,Y1,X2,Y2):-
    \+ board:empty(X2,Y2),
    !,
    X3 is X2 + X2 - X1,
    Y3 is Y2 + Y2 - Y1,
    grasshoperVisit(X2,Y2, X3,Y3).


% Spider

spiderDestination(X1,Y1, X4, Y4):-
    spiderToVisit(X1, Y1, X1,Y1, X2, Y2),
    spiderToVisit(X1, Y1, X2,Y2, X3, Y3),
    spiderToVisit(X1,Y1, X3, Y3, X4, Y4),
    board:cellsAreDistinct([[X1,Y1], [X2,Y2], [X3,Y3], [X4,Y4]]).
    
spiderToVisit(Sx,Sy,X1,Y1,X2,Y2):-
    board:accesibleCell(X1,Y1,X2,Y2),
    board:nonEmptyAdyacent(X2,Y2, X3, Y3),
    board:cellsAreDistinct(Sx,Sy, X3, Y3).

% Ant

antDestinations(X, Y):-
    forall(destination(A,B), retract(destination(A,B))),
    antVisit(X, Y ,X , Y),
    retract(destination(X,Y)).

antVisit(Xs,Ys, Xc, Yc):-
    assert(destination(Xc,Yc)),
    forall(antToVisit(Xs, Ys, Xc, Yc, X1, Y1), antVisit(Xs, Ys, X1, Y1)).

antToVisit(Sx,Sy, X1, Y1, X2, Y2):-
    board:accesibleCell(X1,Y1,X2,Y2),
    \+ destination(X2, Y2),
    board:nonEmptyAdyacent(X2,Y2, X3,Y3),
    board:cellsAreDistinct(Sx,Sy, X3, Y3).

% Ladybug

ladybugDestination(X1,Y1,X4, Y4):-
    board: nonEmptyAdyacent(X1,Y1,X2,Y2),
    board: nonEmptyAdyacent(X2,Y2, X3,Y3),
    board: frontierAdyacent(X3,Y3, X4,Y4),
    board: cellsAreDistinct([[X1,Y1],[X2,Y2], [X3,Y3], [X4,Y4]]).


%Pillbug
% Px,Py : Position of the pillbug
% Lx, Ly : Position of last enemy move 
% Pmove: [X,Y]: move the pillbug to X,Y 
% FromAdyacent: [X,Y] take the piece adyacent to pillbug in position X,Y
% ToAdyacent: [X,Y] can place the piece taken by the pillbug in position X,Y
pillbugDestinations(Px, Py, Lx, Ly, Pmove, FromAdyacent, ToAdyacent):-
    findall([X1,Y1], board:accesibleCell(Px,Py,X1,Y1), Pmove), %Move the pillbug
    findall([X2,Y2], selectPiecesToMove(Px,Py,Lx,Ly,X2,Y2), FromAdyacent), %Select adyacent piece to pillbug
    findall([X3,Y3], board:frontierAdyacent(Px,Py,X3,Y3), ToAdyacent). %Move selected piece To Adyacent Position
    
normalPlays(Px,Py, Moves):-
    findall([X1, Y1], board:accesibleCell(Px, Py, X1, Y1), Moves).

selectPiecesToMove(Px,Py,Lx,Ly, X, Y):-
    board:nonEmptyAdyacent(Px,Py,X,Y),
    board:cellsAreDistinct(X,Y,Lx,Ly),
    board:cellNonStacked(X,Y),
    board:canBeRemoved(X,Y),
    board:adyacent(X,Y, X1,Y1),
    board:adyacent(X,Y, X2,Y2),
    board:adyacent(Px,Py,X1,Y1),
    board:adyacent(Px,Py, X2,Y2),
    board:cellsAreDistinct(X1,Y1,X2,Y2),
    board:cellNonStacked(X1,Y1),
    board:cellNonStacked(X2,Y2).

