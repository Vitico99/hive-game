:-use_module(library(pce)).
:-use_module(board).
:-use_module(bugs).
:-use_module(ai).


:- dynamic selectedBug/3.
:- dynamic drawedPlaceable/3.
:- dynamic opponent/1.
:- dynamic cpuColor/1.
:- dynamic counter/6.

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
    new(@boardCanvas, picture('Board', size(900, 780))),
    new(RightPanel, dialog_group(' ')),
    new(Menu, dialog_group('Main Menu')),
    new(BlackPieces, dialog_group('Black')),
    new(WhitePieces, dialog_group('White')),
    new(StatusBar, dialog_group('Game Status')),
    new(@sview, dialog_group('Stack Viewer')),

    %Main Menu
    new(Opponent, menu(opponent)),
    send_list(Opponent, append, [computer, human]),
    send(Menu, append, Opponent),
    send(Menu, append, button(newGame, message(@prolog, startNewGame, Opponent?selection))),
    send(Menu, append, button(erase, message(@prolog, clearPlaceableCells))),

    %board:initBoard(white, black),
    %assertz(opponent(cpu)),
    %assertz(cpuColor(black)),
    
    % Status bar
    new(@sbar, picture('Sbar', size(260, 70))),
    send(@sbar, background, colour(gray)),
    new(@currentColorBox, box(10,10)),
    send(StatusBar, append, @sbar),

    % StackViewer
    new(@svCanvas, picture('Sview', size(260, 70))),
    send(@svCanvas, background, colour(gray)),
    send(@sview, append, @svCanvas),

    %Black Pieces
    new(@bCanvas, picture('Black Pieces', size(260, 180))),
    send(@bCanvas, background, colour(gray)),
    send(BlackPieces, append, @bCanvas),

    %White Pieces
    new(@wCanvas, picture('White Pieces', size(260, 180))),
    send(@wCanvas, background, colour(gray)),
    send(WhitePieces, append, @wCanvas),

    %Right Panel
    send(RightPanel, append, Menu),
    send(RightPanel, append, StatusBar),
    send(RightPanel, append, BlackPieces),
    send(RightPanel, append, WhitePieces),
    send(RightPanel, append, @sview),

    startNewGame(human),

    send(@boardCanvas, background, colour(gray)),
    assertz(board(@boardCanvas)),
    send(Window, append, @boardCanvas),
    send(Window, append, RightPanel, right),
    send(Window, open).


startNewGame(O):-
    retractall(opponent(_)),
    retractall(cpuColor(_)),
    board:clearBoard,
    board:initBoard(white, black),
    send(@boardCanvas,clear),
    send(@svCanvas,clear),
    drawStatusBar(@sbar),
    drawPieceSelection(white, @wCanvas),
    drawPieceSelection(black, @bCanvas),
    startCpu(O).

startCpu(human).
startCpu(computer):-
    assertz(opponent(computer)),
    assertz(cpuColor(black)). 


drawStatusBar(Canvas):-
    send(Canvas, clear),
    send(@currentColorBox, free),
    send(Canvas, display, new(@currentColorBox, box(10,10)), point(10,10)),
    send(@currentColorBox, fill_pattern, colour(white)),
    send(Canvas, display, new(text('player\'s turn')), point(30, 5)),
    send(Canvas, display, new(text('Selected cell:')), point(10, 30)).

drawPieceSelection(Color, Canvas):-
    send(Canvas, clear),
    retractall(counter(Color,_, _, Canvas, _, _)),
    % get resources
    bm(Color, queen, Qb), bm(Color, beetle, Bb), bm(Color, grasshoper, Gb),
    bm(Color, spider, Sb), bm(Color, ant, Ab), bm(Color, ladybug, Lb),
    bm(Color, mosquito, Mb), bm(Color, pigbull, Pb),
    
    drawPiece(Canvas, Color, queen, 40, 30),
    drawPiece(Canvas, Color, beetle, 115, 30),
    drawPiece(Canvas, Color, grasshoper, 190, 30),
    drawPiece(Canvas, Color, spider, 40, 85),
    drawPiece(Canvas, Color, ant, 115, 85),
    drawPiece(Canvas, Color, ladybug, 190, 85),
    drawPiece(Canvas, Color, mosquito, 40, 140),
    drawPiece(Canvas, Color, pigbull, 115, 140).
   
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
    clearPlaceableCells,
    board:currentColor(Color),
    board:placeableByColor(Color, Type),
    retractall(selectedBug(_,_,_)),
    assertz(selectedBug(Color, Type, place)),
    updateSelectedCell,
    drawPlaceableCells(Color).


% Event for drawing the empty cells for placing a bug
drawPlaceableCells(Color):-
    clearPlaceableCells,
    forall(board:placeableByColor(X1,Y1,Color), drawPlaceableCell(X1,Y1,_)).

drawPlaceableCell(X, Y, Cell):-
    board(Board), % Get the resources
    translate(X,Y,X1,Y1),
    hexagon(X1,Y1,Cell,lightgreen),
    send(Cell, recogniser, click_gesture(left, '', single, message(@prolog, drawBugCell, X , Y))),
    send(Board, display, Cell), 
    assertz(drawedPlaceable(X,Y,Cell)).

drawDestinationCells(C, T, X1, Y1, Cell, B):-
    clearPlaceableCells,
    board:canBeRemoved(X1,Y1),
    board:canBeMoved(X1, Y1, 0),
    retractall(selectedBug(_,_,_)),
    assertz(selectedBug(C, T, move)),
    updateSelectedCell,
    write_ln(T),
    bugs:getDestinations(X1,Y1,T,C),
    forall(bugs:destination(X2,Y2), drawDestinationCell(X1,Y1,X2,Y2,Cell,B)).

drawDestinationCell(X1,Y1,X2,Y2,BugCell,B):-
    board(Board), % Get the resources
    translate(X2,Y2,X3,Y3),
    hexagon(X3,Y3,Cell,lightgreen),
    send(Cell, recogniser, click_gesture(left, '', single, message(@prolog, moveBug, X1, Y1, X2, Y2, BugCell, B))),
    send(Board, display, Cell),
    assertz(drawedPlaceable(X2,Y2,Cell)).

moveBug(X1,Y1,X2,Y2,BugCell, B):-
    clearPlaceableCells,
    board:getCellTop(X1,Y1,S), 
    drawedBug(X1,Y1, S, Cell1, B1),
    send(Cell1, free), 
    send(B1, free),
    retract(drawedBug(X1,Y1,S,Cell1, B1)),
    board:removeBug(X1,Y1),
    drawBugCell(X2,Y2).
    
makeCpuMove():- 
    opponent(computer),
    board:currentColor(C1), 
    cpuColor(C2),
    C1 == C2,
    ai:alphaBeta(C1,M,S),
    %cpu:minimax(C1, 2, maximize, _, M),
    write_ln('here'),
    makeCpuMove(M).
makeCpuMove().

makeCpuMove([T,X,Y]):-
    cpuColor(C),
    retractall(selectedBug(_,_,_)),
    assertz(selectedBug(C, T, place)),
    drawBugCell(X,Y).
makeCpuMove([T,X1,Y1,X2,Y2]):-
    cpuColor(C),
    retractall(selectedBug(_,_,_)),
    assertz(selectedBug(C,T,move)),
    moveBug(X1, Y1, X2, Y2, a, a).

drawBugCell(X, Y):- %add another mode like place/move to use the line that updates the counter
    selectedBug(C, T, _), % Get the bug that is going to be drawn
    board(Board), bm(C, T, Bm), % Get the resources
    translate(X,Y,X1,Y1),
    hexagon(X1,Y1,Cell,C),
    new(B, bitmap(Bm)),
    send(Board, display, Cell),
    send(Cell, recogniser, click_gesture(left, '', single, message(@prolog, drawDestinationCells, C, T, X, Y, Cell, B))),
    send(Cell, recogniser, click_gesture(right, '', single, message(@prolog, drawStack, X, Y))),
    send(Board, display, B, point(X1-21, Y1-12)),
    board:placeBug(C,T,X,Y),
    (   %if we draw a bug that was moved then we don't need to update the counters
        selectedBug(_,_,move); 
        (board:updateBugCount(C,T), updateCounter(C,T))
    ),
    board:getCellTop(X,Y,S),
    assertz(drawedBug(X,Y,S,Cell, B)),
    board:changeCurrentColor,
    updateCurrentColorBox,
    clearPlaceableCells,
    makeCpuMove.

drawStack(X,Y):-
    send(@svCanvas, clear),
    forall(board:bug(C,T,X,Y,S),  drawStackedCell(C,T,S)).

drawStackedCell(C,T,S):-
    X is 30 + 60 * S,
    hexagon(X, 30, H, C),
    bm(C,T,Bm),
    new(B, bitmap(Bm)),
    send(@svCanvas, display, H),
    send(@svCanvas, display, B, point(X-21, 18)).


updateSelectedCell:-
    selectedBug(Color, Type, _),
    hexagon(130,35,H,Color),
    send(@sbar, display, H),
    bm(Color,Type, Bm),
    new(B, bitmap(Bm)),
    send(@sbar, display, B, point(109,23)).


updateCurrentColorBox:-
    board:currentColor(C),
    send(@currentColorBox, fill_pattern, colour(C)). 

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
    