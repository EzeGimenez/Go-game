:- module(proylcc,
	[  
		emptyBoard/1,
		goMove/5
	]).

emptyBoard([
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"]]).

% goMove(+Board, +Player, +Pos, -RBoard)
%
% RBoard is the result configuration of reflection the move of the player at Pos using the existent Board configuration
goMove(Board, Player, [R,C], RBoard, RBoard):-
    justMove(Board, "-", Player, [R, C], Aux),
    opuesto(Player,Op),
    %makes an inverse move so it checks if el movimiento anterior fue sobre un espacio en blanco

    refresh(Aux, [0,0], Aux, RBoard, Op),
    (
    	notSuicidalMove(Board, Player, [R, C])
    	; 
    	(justMove(RBoard, _ , "-", [R,C], NewBoard), notSuicidalMove(NewBoard, Player, [R, C]))
    	).

%makes a move without checking if it's suicide 
justMove(Board, OrigC, Player, [R,C], RBoard):-
	replace(Row, R, NRow, Board, RBoard),	% Row es la buscada, R vamos decrementando, 
    									% NRow es la fila resultante, Board tablero y RBoard el tablero resultante
    replace(OrigC, C, Player, Row, NRow).

% Y es lo que queda
% XIndex es el que va restando
% X es el elemento buscado y a remplazar por Y
% replace(?X, +XIndex, +Y, +Xs, -XsY)
replace(X, 0, Y, [X|Xs], [Y|Xs]).
replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%shell del getValue, devuelve en Colour la ficha buscada
getValueS(Board, [R, C], Color) :-
	getValue(R, Board, Row),
	getValue(C, Row, Color).

getValue(0, [X|_Xs], X).
getValue(XIndex, [_Xi|Xs], X):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    getValue(XIndexS, Xs, X).

adjacent([R, C], [R, C1]) :- C < 18, C1 is (C + 1).
adjacent([R, C], [R, C1]) :- C > 0, C1 is (C - 1).
adjacent([R, C], [R1, C]) :- R < 18, R1 is (R + 1).
adjacent([R, C], [R1, C]) :- R > 0, R1 is (R - 1).

surrounded(Board, [R, C], Visited) :-
	getValueS(Board, [R, C], ValueOfCurrent),
	ValueOfCurrent \= "-",
	findall(X, adjacent([R, C], X), Adjacents),
	% Check if "-" doesn't belong to Adjacents
	% First we must search for the value of each adjacent & then compare.
	getValuesOfPoints(Board, Adjacents, Values),
	not(member("-", Values)),
	% If there's no value surrounding [R, C] such that it's the same as [R, C]'s value, then [R, C] is surrounded,
	% otherwise we should continue recursion with each of the values.
	findall(Pos, (member(Pos, Adjacents), getValueS(Board, Pos, ValueOfPos), ValueOfCurrent = ValueOfPos), SameColors),
	findall(X, (member(X, SameColors), not(member(X, Visited))), ToVisit),
	callSurroundedForAllSameColorAdjacent(Board, ToVisit, [[R, C] | Visited]).

getValuesOfPoints(_Board, [], []).
getValuesOfPoints(Board, [X | Xs], [N | Ns]) :- getValuesOfPoints(Board, Xs, Ns), getValueS(Board, X, N).

% Generalize with call\2+
% Call surrounded method for all Adjacents that are and not visited.
callSurroundedForAllSameColorAdjacent(_Board, [], _Visited).
callSurroundedForAllSameColorAdjacent(Board, [X | Xs], Visited) :- 
	surrounded(Board, X, Visited),
	callSurroundedForAllSameColorAdjacent(Board, Xs, Visited).

% Full grid traversal in search of surrounded colors.
refresh(_Board, _, [], [], Color).
refresh(Board, [R, C], [X | Xs], [M | NBoard], Color) :- 
	R1 is R + 1,
	refresh(Board, [R1, C], Xs, NBoard, Color),
	refreshRow(Board, [R, 0], X, M, Color).

refreshRow(_Board, [_R, _C], [], [], Color).
refreshRow(Board, [R, C], [_X | Xs], ["-" | NFila], Color) :- 
	getValueS(Board, [R,C], Color),
	surrounded(Board, [R, C], []),
	C1 is C + 1,
	refreshRow(Board, [R, C1], Xs, NFila, Color).

refreshRow(Board, [R, C], [X | Xs], [X | NFila], Color) :-
	C1 is C + 1,
	refreshRow(Board, [R, C1], Xs, NFila, Color).

%true si es suicida false caso contrario	
notSuicidalMove(Board, Player, [R, C]) :-
	replace(Row, R, NRow, Board, FakeBoard),
    replace("-", C, Player, Row, NRow),
	not(surrounded(FakeBoard, [R,C], [])).

%predicado que calcule si dada la ficha [R,C] colocada, alguno de sus adyacentes es cerrado por el color de la ficha 
%si alguno de los adyacentes es cerrado ya es verdadero
isAnyOfMyAdjacentsSorrounded(Board, [R, C],RBoard):-
	findall(X, adjacent([R, C], X), Adjacents), 	
	surrounded(Board, [R, C], Adjacents).
