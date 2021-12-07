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
getDestinations(X,Y,grasshoper,C):-
    board:currentColor(C),
    forall(destination(A,B), retractall(destination(A,B))),
    grasshoperDestinations(X,Y),
    (overPillbugDestinations(X,Y); true).

getDestinations(X,Y,ant,C):-
    board:currentColor(C),
    forall(destination(A,B), retractall(destination(A,B))),
    antDestinations(X,Y),
    (overPillbugDestinations(X,Y); true).

getDestinations(X,Y,_,_):-
    write_ln('toy aqui puta'),
    forall(destination(A,B), retractall(destination(A,B))),
    (overPillbugDestinations(X,Y); true).


% isPossibleDestination/5
isPossibleDestination(X1, Y1, X2, Y2, queen, C):- % Queen
    board:currentColor(C),
    board:accesibleCell(X1, Y1, X2, Y2).

isPossibleDestination(X1, Y1, X2, Y2, beetle, C):- % Beetle
    board:currentColor(C),
    (board:accesibleCell(X1,Y1,X2,Y2);
    board:nonEmptyAdyacent(X1,Y1,X2,Y2)).

isPossibleDestination(X1, Y1, X2, Y2, spider, C):- % Spider
    board:currentColor(C),
    spiderDestination(X1, Y1, X2, Y2).


isPossibleDestination(X1, Y1, X2, Y2, ladybug, C):- % Ladybug
    board:currentColor(C),
    ladybugDestination(X1, Y1, X2, Y2).

isPossibleDestination(X1, Y1, X2, Y2, pigbull, C):-
    board:currentColor(C),
    board:accesibleCell(X1, Y1, X2, Y2).

isPossibleDestination(_, _, X2, Y2, _, _):- % Ant
    destination(X2,Y2).

% ============================== Implemenation =================================
% Implementation of more complex moves generators

% Grasshoper

grasshoperDestinations(X, Y):-
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
    board:adyacent(X,Y, X1,Y1),
    board:adyacent(X,Y, X2,Y2),
    board:cellsAreDistinct(X1,Y1,X2,Y2),
    board:adyacent(Px,Py,X1,Y1),
    board:adyacent(Px,Py,X2,Y2),
    board:cellNonStacked(X1,Y1),
    board:cellNonStacked(X2,Y2).

overPillbugDestinations(X,Y):-
    % Check if (X,Y) has a friendly pigbull adyacent to it
    board:currentColor(C1), 
    board:adyacent(X, Y, X1, Y1), % (X1, Y1) is the location of the pigbull
    board:bug(C1,pigbull,X1,Y1,0),
    write_ln('encontre el pigbull'),

    board:opponent(C1, C2), % Check (X,Y) is not the last piece moved by the opponent
    \+ board:lastPlacedBug(C2,_,X,Y,_),

    board:cellNonStacked(X,Y),

    % search for the two common adyacents of (X,Y) and (X1, Y1)
    board:adyacent(X1, Y1, X2, Y2), 
    board:adyacent(X, Y, X2, Y2),
    
    board:adyacent(X1, Y1, X3, Y3),
    board:adyacent(X, Y, X3, Y3),

    board:cellsAreDistinct(X2,Y2,X3,Y3),

    board:cellNonStacked(X2,Y2),
    board:cellNonStacked(X3,Y3),

    forall(board:emptyAdyacent(X1,Y1,X4,Y4), assertz(destination(X4,Y4))).
