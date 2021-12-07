:-module(bugs, []).
:-use_module(board).

:- dynamic destination/2.
:- dynamic visited/2.

% Queen

queenDestinations(X, Y, D):-
    findall([X1, Y1], board:accesible_cell(X, Y, X1, Y1), D).

% Beetle

beetleDestinations(X, Y, D):-
    findall([X1, Y1],beetleToVisit(X, Y, X1, Y1), D).

beetleToVisit(X1, Y1, X2, Y2):-
    board:accesible_cell(X1, Y1, X2, Y2);
    board:nonEmptyAdyacent(X1, Y1, X2, Y2).


% Grasshoper

grasshoperDestinations(X, Y, D):-
    forall(destination(A,B), retract(destination(A,B))),
    forall(board:nonEmptyAdyacent(X, Y, X1, Y1), grasshoperVisit(X, Y, X1, Y1)),
    findall([X2,Y2], destination(X2,Y2), D).


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

spiderDestinations(X, Y, D):-
    findall([X1,Y1], spiderDestinations(X,Y, X1,Y1), D).

spiderDestinations(X1,Y1, X4, Y4):-
    canGoTo(X1, Y1, X1,Y1, X2, Y2),
    canGoTo(X1, Y1, X2,Y2, X3, Y3),
    canGoTo(X1,Y1, X3, Y3, X4, Y4),
    board:cellsAreDistinct([[X1,Y1], [X2,Y2], [X3,Y3], [X4,Y4]]).
    
canGoTo(Sx,Sy,X1,Y1,X2,Y2):-
    board:accesible_cell(X1,Y1,X2,Y2),
    board:nonEmptyAdyacent(X2,Y2, X3, Y3),
    board:cellsAreDistinct(Sx,Sy, X3, Y3).

% Ant

antDestinations(X, Y, D):-
    forall(visited(A,B), retract(visited(A,B))),
    antVisit(X, Y ,X , Y),
    retract(visited(X,Y)),
    findall([X1,Y1], visited(X1,Y1), D).

antVisit(Xs,Ys, Xc, Yc):-
    assert(visited(Xc,Yc)),
    forall(antToVisit(Xs, Ys, Xc, Yc, X1, Y1), antVisit(Xs, Ys, X1, Y1)).

antToVisit(Sx,Sy, X1, Y1, X2, Y2):-
    board:accesible_cell(X1,Y1,X2,Y2),
    \+ visited(X2, Y2),
    board:nonEmptyAdyacent(X2,Y2, X3,Y3),
    board:cellsAreDistinct(Sx,Sy, X3, Y3).


% Ladybug
ladybugDestinations(X,Y,D):-
    findall([X1,Y1],ladybugDestinations(X,Y,X1,Y1), D).

ladybugDestinations(X1,Y1,X4, Y4):-
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
    findall([X1,Y1], board:accesible_cell(Px,Py,X1,Y1), Pmove), %Move the pillbug
    findall([X2,Y2], selectPiecesToMove(Px,Py,Lx,Ly,X2,Y2), FromAdyacent), %Select adyacent piece to pillbug
    findall([X3,Y3], board:frontierAdyacent(Px,Py,X3,Y3), ToAdyacent). %Move selected piece To Adyacent Position
    
normalPlays(Px,Py, Moves):-
    findall([X1, Y1], board:accesible_cell(Px, Py, X1, Y1), Moves).

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
