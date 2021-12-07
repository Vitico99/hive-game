:-use_module(library(pce)).
:-use_module(board).
:-use_module(bugs).
:- dynamic selectedBug/3.
:- dynamic drawedPlaceable/3.

% Resources 45x25px
bm(black, queen, './xpm/BQ.xpm').
bm(black, beetle, './xpm/BB.xpm'). 
bm(black, grasshoper, './xpm/BG.xpm').
bm(black, spider, './xpm/BS.xpm').
bm(black, ant, './xpm/BA.xpm').
bm(black, ladybug, './xpm/BL.xpm').
bm(black, mosquito, './xpm/BM.xpm').
bm(black, pigbull, './xpm/BP.xpm').
bm(white, queen, './xpm/WQ.xpm').
bm(white, beetle, './xpm/WB.xpm'). 
bm(white, grasshoper, './xpm/WG.xpm').
bm(white, spider, './xpm/WS.xpm').
bm(white, ant, './xpm/WA.xpm').
bm(white, ladybug, './xpm/WL.xpm').
bm(white, mosquito, './xpm/WM.xpm').
bm(white, pigbull, './xpm/WP.xpm').
bm(empty, './xpm/E.xpm').
title(white, 'White Pieces').
title(black, 'Black Pieces').





app():-
    new(Window, dialog('Hive')),
    new(Board, picture('Board', size(900, 780))),
    new(RightPanel, dialog_group(' ')),
    new(Menu, dialog_group('Main Menu')),
    new(BlackPieces, dialog_group('Black')),
    new(WhitePieces, dialog_group('White')),

    %Main Menu
    new(Opponent, menu(opponent)),
    send_list(Opponent, append, [computer, human]),
    send(Menu, append, Opponent),
    send(Menu, append, button(newGame, message(@prolog, drawPlaceableCells, black))),
    send(Menu, append, button(erase, message(@prolog, clearPlaceableCells))),

    board:initBoard(white, black),
    
    %Black Pieces
    drawPieceSelection(black, BCanvas),
    send(BlackPieces, append, BCanvas),

    %White Pieces
    drawPieceSelection(white, WCanvas),
    send(WhitePieces, append, WCanvas),

    %Right Panel
    send(RightPanel, append, Menu),
    send(RightPanel, append, BlackPieces),
    send(RightPanel, append, WhitePieces),
    
    %initialization


    send(Board, background, colour(gray)),

    assertz(board(Board)),
    send(Window, append, Board),
    send(Window, append, RightPanel, right),
    send(Window, open).

drawPieceSelection(Color, Canvas):-
    title(Color, T),
    new(Canvas, picture(T, size(260, 220))),
    send(Canvas, background, colour(gray)),

    % get resources
    bm(Color, queen, Qb), bm(Color, beetle, Bb), bm(Color, grasshoper, Gb),
    bm(Color, spider, Sb), bm(Color, ant, Ab), bm(Color, ladybug, Lb),
    bm(Color, mosquito, Mb), bm(Color, pigbull, Pb),
    
    drawPiece(Canvas, Color, queen, 40, 30),
    drawPiece(Canvas, Color, beetle, 115, 30),
    drawPiece(Canvas, Color, grasshoper, 190, 30),
    drawPiece(Canvas, Color, spider, 40, 105),
    drawPiece(Canvas, Color, ant, 115, 105),
    drawPiece(Canvas, Color, ladybug, 190, 105),
    drawPiece(Canvas, Color, mosquito, 40, 180),
    drawPiece(Canvas, Color, pigbull, 115, 180).
   
drawPiece(Canvas, Color, Type, X, Y):-
    bm(Color, Type, Bm),
    hexagon(X,Y, Cell, Color),
    send(Cell, recogniser, click_gesture(left, '', single, message(@prolog, selectBugForPlacement, Color, Type))),
    send(Canvas, display, Cell),
    send(Canvas, display, new(bitmap(Bm)), point(X-20,Y-12)),
    board:availableBug(Color, Type, Cnt),
    send(Canvas, display, new(text('x')), point(X+26,Y)),
    send(Canvas, display, new(Text, text(Cnt)), point(X+33,Y)),
    assertz(counter(Color, Type, Text, Canvas, X, Y)).

updateCounter(Color, Type):-
    board:availableBug(Color, Type, Cnt),
    counter(Color, Type, Text, Canvas, X, Y),
    send(Text, free),
    send(Canvas, display, new(Text1, text(Cnt)), point(X+33, Y)),
    retract(counter(Color, Type, Text, Canvas, X, Y)),
    assertz(counter(Color, Type, Text1, Canvas, X, Y)).



% ================================== Events ==================================================


selectBugForPlacement(Color, Type):-
    board:currentColor(Color),
    board:availableBug(Color, Type, Cnt),
    Cnt > 0,
    retractall(selectedBug(_,_,_)),
    assertz(selectedBug(Color, Type, place)),
    drawPlaceableCells(Color).


% Event for drawing the empty cells for placing a bug
drawPlaceableCells(Color):-
    clearPlaceableCells, %TODO: fix this shit
    forall(board:placeableByColor(X1,Y1,Color), drawPlaceableCell(X1,Y1,_)).

drawPlaceableCell(X, Y, Cell):-
    board(Board), % Get the resources
    translate(X,Y,X1,Y1),
    hexagon(X1,Y1,Cell,lightgreen),
    send(Cell, recogniser, click_gesture(left, '', single, message(@prolog, drawBugCell, X , Y))),
    send(Board, display, Cell), 
    assertz(drawedPlaceable(X,Y,Cell)).

drawDestinationCells(C, T, X1, Y1, Cell, B):-
    board:canBeRemoved(X1,Y1),
    retractall(selectedBug(_,_,_)),
    assertz(selectedBug(C, T, move)),
    write_ln(T),
    bugs:getDestinations(X1,Y1,T),
    write_ln('here'),
    forall(bugs:isPossibleDestination(X1,Y1,X2,Y2,T), drawDestinationCell(X1,Y1,X2,Y2,Cell,B)).

drawDestinationCell(X1,Y1,X2,Y2,BugCell,B):-
    board(Board), % Get the resources
    translate(X2,Y2,X3,Y3),
    hexagon(X3,Y3,Cell,lightgreen),
    send(Cell, recogniser, click_gesture(left, '', single, message(@prolog, moveBug, X1, Y1, X2, Y2, BugCell, B))),
    send(Board, display, Cell),
    assertz(drawedPlaceable(X2,Y2,Cell)).

moveBug(X1,Y1,X2,Y2,BugCell, B):-
    clearPlaceableCells, 
    send(BugCell, free), 
    send(B, free),
    board:removeBug(X1,Y1),
    drawBugCell(X2,Y2).
    

drawBugCell(X, Y):- %add another mode like place/move to use the line that updates the counter
    selectedBug(C, T, _), % Get the bug that is going to be drawn
    write_ln('voa pintar'),
    board(Board), bm(C, T, Bm), % Get the resources
    translate(X,Y,X1,Y1),
    hexagon(X1,Y1,Cell,C),
    new(B, bitmap(Bm)),
    send(Board, display, Cell),
    send(Cell, recogniser, click_gesture(left, '', single, message(@prolog, drawDestinationCells, C, T, X, Y, Cell, B))),
    send(Board, display, B, point(X1-21, Y1-12)),
    board:placeBug(C,T,X,Y),
    (   %if we draw a bug that was moved then we don't need to update the counters
        selectedBug(_,_,move); 
        (board:updateBugCount(C,T), updateCounter(C,T))
    ),
    board:changeCurrentColor,
    clearPlaceableCells.

clearPlaceableCells():-
    forall(drawedPlaceable(X,Y,Cell), clearPlaceableCell(X,Y,Cell)).

clearPlaceableCell(X, Y, Cell):-
    retract(drawedPlaceable(X, Y, Cell)),
    send(Cell, free).

closes(Window):-
    send(Window, free).

% Graphics methods
  
hexagon(X, Y, H, C):-
    new(H, path),
    send(H, append, point(X + -25, Y)),
    send(H, append, point(X + -12.5, Y + 21.65)),
    send(H, append, point(X + 12.5, Y + 21.65)),
    send(H, append, point(X + 25, Y)),
    send(H, append, point(X + 12.5, Y + -21.65)),
    send(H, append, point(X + -12.5, Y + -21.65)),
    send(H, colour, colour(C)),
    send(H, fill_pattern, colour(C)).

translate(X1,Y1,X2,Y2):-
    X2 is X1 * 37.5,
    Y2 is X1 * 21.65 + Y1 * 43.3.
    