:-module(bugs, []).
:-use_module(board).

:- dynamic getDestinations/4.
:- dynamic destination/2.


% ================================= API ====================================
% This section provides two predicates to obtain the possible moves for a bug:
% - getDestinations/4: X, Y, T, C computes the possible moves of a bug of type T and color C from cell (X,Y).
% - destination/2: can be used as a generator to get all possible moves


% getDestinations/4
getDestinations(X,Y,T,C):-
    board:currentColor(C),
    forall(destination(A,B), retractall(destination(A,B))),
    bugDestinations(X,Y,T),
    (overPillbugDestinations(X,Y); write_ln('Generated possible moves 1')).

getDestinations(X,Y,_,_):-
    forall(destination(A,B), retractall(destination(A,B))),
    (overPillbugDestinations(X,Y); write_ln('Generated possible moves 2')).

bugDestinations(X,Y,queen):- queenDestinations(X,Y).
bugDestinations(X,Y,beetle):- beetleDestinations(X,Y).
bugDestinations(X,Y,grasshoper):- grasshoperDestinations(X,Y).
bugDestinations(X,Y,spider):- spiderDestinations(X,Y).
bugDestinations(X,Y,ant):- antDestinations(X,Y).
bugDestinations(X,Y,ladybug):- ladybugDestinations(X,Y).
bugDestinations(X,Y,pigbull):- pigbullDestinations(X,Y).

% ============================== Implemenation =================================

% Queen

queenDestinations(X,Y):-
    forall(board:accesibleCell(X,Y,X1,Y1), assertz(destination(X1,Y1))).

% Beetle

beetleDestinations(X,Y):-
    forall(beetleDestination(X,Y,X1,Y1), assertz(destination(X1,Y1))).

beetleDestination(X1,Y1,X2,Y2):-
    board:accesibleCell(X1,Y1,X2,Y2);
    board:nonEmptyAdyacent(X1,Y1,X2,Y2).

% Grasshoper

grasshoperDestinations(X, Y):-
    forall(board:nonEmptyAdyacent(X, Y, X1, Y1), grasshoperVisit(X, Y, X1, Y1)).

grasshoperVisit(_,_,X2,Y2):-
    board:empty(X2,Y2),!,
    assert(destination(X2,Y2)).
grasshoperVisit(X1,Y1,X2,Y2):-
    \+ board:empty(X2,Y2), !,
    X3 is X2 + X2 - X1,
    Y3 is Y2 + Y2 - Y1,
    grasshoperVisit(X2,Y2, X3,Y3).

% Spider

spiderDestinations(X, Y):-
    forall(spiderDestination(X,Y,X1,Y1), assertz(destination(X1,Y1))).

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

ladybugDestinations(X, Y):-
    forall(ladybugDestination(X,Y,X1,Y1), assertz(destination(X1,Y1))).

ladybugDestination(X1,Y1,X4, Y4):-
    board: nonEmptyAdyacent(X1,Y1,X2,Y2),
    board: nonEmptyAdyacent(X2,Y2, X3,Y3),
    board: frontierAdyacent(X3,Y3, X4,Y4),
    board: cellsAreDistinct([[X1,Y1],[X2,Y2], [X3,Y3], [X4,Y4]]).

% Pigbull

pigbullDestinations(X,Y):-
    forall(board:accesibleCell(X,Y,X1,Y1), assertz(destination(X1,Y1))).

overPillbugDestinations(X,Y):-
    % Check if (X,Y) has a friendly pigbull adyacent to it
    board:currentColor(C1), 
    board:adyacent(X, Y, X1, Y1), % (X1, Y1) is the location of the pigbull
    board:bug(C1,pigbull,X1,Y1,0),

    board:opponent(C1, C2), % Check (X,Y) is not the last piece moved by the opponent
    \+ board:lastPlacedBug(C2,_,_,X,Y,_),

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
