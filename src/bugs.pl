:-module(bugs, []).
:-use_module(board).

:- dynamic getDestinations/4.
:- dynamic destination/2.
:- dynamic mosquitoDestination/2.
:- dynamic mosquitoCopied/1.

% ================================= API ====================================
% This section provides two predicates to obtain the possible moves for a bug:
% - getDestinations/4: X, Y, T, C computes the possible moves of a bug of type T and color C from cell (X,Y).
% - destination/2: can be used as a generator to get all possible moves


% getDestinations/4
getDestinations(X,Y,T,C):-
    board:currentColor(C),
    forall(destination(A,B), retractall(destination(A,B))),
    bugDestinations(X,Y,T),
    (overPillbugDestinations(X,Y); true).

getDestinations(X,Y,_,_):-
    forall(destination(A,B), retractall(destination(A,B))),
    (overPillbugDestinations(X,Y); true).

bugDestinations(X,Y,queen):- queenDestinations(X,Y).
bugDestinations(X,Y,beetle):- beetleDestinations(X,Y).
bugDestinations(X,Y,grasshoper):- grasshoperDestinations(X,Y).
bugDestinations(X,Y,spider):- spiderDestinations(X,Y).
bugDestinations(X,Y,ant):- antDestinations(X,Y).
bugDestinations(X,Y,ladybug):- ladybugDestinations(X,Y).
bugDestinations(X,Y,pigbull):- pigbullDestinations(X,Y).
bugDestinations(X,Y,mosquito):- mosquitoDestinations(X,Y).

countDestinations(X,Y, T, Count):-
    forall(destination(A,B), retractall(destination(A,B))),
    bugDestinations(X,Y,T),
    aggregate_all(count,destination(_,_), Count).

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
    board:bug(C,T,X,Y,0), retract(board:bug(C,T,X,Y,0)),
    forall(spiderDestination(X,Y,X1,Y1), assertz(destination(X1,Y1))),
    assertz(board:bug(C,T,X,Y,0)).

spiderDestination(X1,Y1, X4, Y4):-
    spiderToVisit(X1,Y1, X2, Y2),
    spiderToVisit(X2,Y2, X3, Y3),
    spiderToVisit(X3, Y3, X4, Y4),
    board:cellsAreDistinct([[X1,Y1], [X2,Y2], [X3,Y3], [X4,Y4]]).
    
spiderToVisit(X1,Y1,X2,Y2):-
    board:accesibleCell(X1,Y1,X2,Y2).

% Ant

antDestinations(X, Y):-
    board:bug(C,T,X,Y,S), retract(board:bug(C,T,X,Y,S)),
    antVisit(X, Y),
    retract(destination(X,Y)), assertz(board:bug(C,T,X,Y,S)).

antVisit(X, Y):-
    assertz(destination(X,Y)),
    forall(antToVisit(X,Y,X1,Y1), antVisit(X1,Y1)).

antToVisit(X1, Y1, X2, Y2):-
    board:accesibleCell(X1,Y1,X2,Y2),
    \+ destination(X2,Y2).

% Ladybug

ladybugDestinations(X, Y):-
    forall(ladybugDestination(X,Y,X1,Y1), assertz(destination(X1,Y1))).

ladybugDestination(X1,Y1,X4, Y4):-
    board: nonEmptyAdyacent(X1,Y1,X2,Y2),
    board: nonEmptyAdyacent(X2,Y2, X3,Y3),
    board: frontierAdyacent(X3,Y3, X4,Y4),
    board: cellsAreDistinct([[X1,Y1],[X2,Y2], [X3,Y3], [X4,Y4]]).

% Mosquito

mosquitoDestinations(X,Y):-
    board: getCellTop(X,Y,S),
    forall(mosquitoCopied(T), retract(mosquitoCopied(T))),
    mosquitoDestinations(X,Y,S).

mosquitoDestinations(X,Y,0):-
    forall(mosquitoDestination(X1,Y1), retract(mosquitoDestination(X1,Y1))),
    forall(mosquitoTarget(X,Y,T), mosquitoCopyBug(X,Y,T)),
    forall(mosquitoDestination(X2,Y2), assertz(destination(X2,Y2))).
mosquitoDestinations(X,Y,_):-
    beetleDestinations(X,Y).

mosquitoTarget(X,Y,T):-
    board: nonEmptyAdyacent(X,Y,X1,Y1),
    board: getCellTop(X1,Y1,S),
    board: bug(_,T,X1,Y1,S).

mosquitoCopyBug(_,_,mosquito).
mosquitoCopyBug(X,Y,T):-
    \+ mosquitoCopied(T),
    forall(destination(A,B), retract(destination(A,B))),
    bugDestinations(X,Y,T),
    forall(destination(X1,Y1), assertz(mosquitoDestination(X1,Y1))),
    assertz(mosquitoCopied(T)).
mosquitoCopyBug(_,_,T):-
    mosquitoCopied(T).

% Pigbull

pigbullDestinations(X,Y):-
    forall(board:accesibleCell(X,Y,X1,Y1), assertz(destination(X1,Y1))).

overPillbugDestinations(X,Y):-
    % Check if (X,Y) has a friendly pigbull adyacent to it
    board:currentColor(C1), 
    board:adyacent(X, Y, X1, Y1), % (X1, Y1) is the location of the pigbull
    %board:bug(C1,pigbull,X1,Y1,0),
    isPigbullLike(X1,Y1,C1),

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

isPigbullLike(X,Y,C):-
    board:bug(C,pigbull,X,Y,0).
isPigbullLike(X,Y,C):-
    board:bug(C,mosquito,X,Y,0),
    mosquitoTarget(X,Y,pigbull).