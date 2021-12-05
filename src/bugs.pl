:-module(bugs, []).
:-use_module(board).

% Queen

queenDestinations(X, Y, D):-
    findall([X1, Y1], board:placeableAdyacent(X, Y, X1, Y1), D).

% Beetle

beetleDestinations(X, Y, D):-
    findall([X1, Y1], board:beetleToVisit(X, Y, X1, Y1), D).

beetleToVisit(X1, Y1, X2, Y2):-
    board:placeableAdyacent(X1, Y1, X2, Y2);
    board:nonEmptyAdyacent(X1, Y1, X2, Y2).


% Grasshoper

grasshoperDestinations(X, Y, D):-
    forall(nonEmptyAdyacent(X, Y, X1, Y1), grasshoperVisit(X, Y, X1, Y1, D)).

grasshoperVisit(X1, Y1, X2, Y2, D):-
    X3 is X2 + X2 - X1,
    Y3 is Y2 + Y2 - Y1,
    ((board:empty(X3, Y3), append([X3, Y3], D));
    grasshoperVisit(X2, Y2, X3, Y3, D)).



% Spider

spiderDestinations(X, Y, D):-
    spiderToVisit(X, Y, 0),
    findall([X1, Y1], destination(X1, Y1), D).

spiderVisit(X, Y, S):-
    assert(visited(X,Y)),
    retract(destination(X,Y)),
    S1 is S + 1,
    forall(spiderToVisit(X, Y, X1, Y1), spiderVisit(X1, Y1, S1)).

spiderVisit(X, Y, 3):-
    assert(destination(X,Y)).

spiderToVisit(X1, Y1, X2, Y2):-
    board:placebaleAdyacent(X1, Y1, X2, Y2),
    \+ visited(X2, Y2).


% Ant

antDestinations(X, Y, D):-
    antVisit(X, Y),
    retract(visited(X,Y)),
    findall([X1,Y1], visited(X1,Y1), D).

antVisit(X,Y):-
    assert(visited(X,Y)),
    forall(antToVisit(X, Y, X1, Y1), antVisit(X1, Y1)).

antToVisit(X1, Y1, X2, Y2):-
    board:placeableAdyacent(X1, Y1, X2, Y2),
    \+ visited(X2, Y2),
    board:placeableAdyacent(X1, Y1, X3, Y3), % Checks at most 6 cells
    board:adyacent(X2, Y2, X3, Y3).


% Ladybug

ladybugDestinations(X, Y, D):-
    ladybugVisit(X, Y, 0),

ladybugVisit(X, Y, S):-
    assert(visited(X, Y)),
    forall(ladybugToVisit(X, Y, X1, Y1, S), ladybugVisit(X1, Y1)).

ladybugVisit(X, Y, 3):-
    assert(destination(X, Y)).

ladybugToVisit(X1, Y1, X2, Y2, S):-
    board:nonEmptyAdyacent(X1, Y1, X2, Y2),
    \+ visited(X2, Y2).

ladybugToVisit(X1, Y1, X2, Y2, 2):-
    board:placeableAdyacent(X1, Y1, X2, Y2).
    \+ visited(X2, Y2).


