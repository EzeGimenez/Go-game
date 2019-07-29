:- [grilla].

score(Board, WhiteScore, BlackScore) :-
	points(Board, [0,0], Board, WhiteScore, BlackScore, _).

points(_Board, _, [], 0, 0, []).
points(Board, [R, C], [X | Xs], W, B, Visited) :-
	R1 is R + 1,
	points(Board, [R1, C], Xs, W1, B1, Aux),
	pointsRow(Board, [R, C], X, W2, B2, Aux2),
	append(Aux, Aux2, Visited),
	W is W1 + W2,
	B is B1 + B2.

pointsRow(_Board, _, [], 0, 0, []).

% Si es color negro, sumamos 1 a la cantidad de negros y seguimos
pointsRow(Board, [R, C], ["b" | Xs], W, B, Visited) :-
	C1 is C + 1,
	pointsRow(Board, [R, C1], Xs, W, B1, Visited),
	B is B1 + 1.

% Si es color blanco, sumamos 1 a la cantidad de blancos y seguimos
pointsRow(Board, [R, C], ["w" | Xs], W, B, Visited) :-
	C1 is C + 1,
	pointsRow(Board, [R, C1], Xs, W1, B, Visited),
	W is W1 + 1.

% Si [R, C] es "-", chequeamos si forma parte de un territorio 
% blanco o negro, sumamos al contador correspondiente, si lo es,
% el tamaño del territorio (las posiciones del territorio están almacenadas en Group)
% y añadimos el territorio a los visitados
pointsRow(Board, [R, C], [_X | Xs], W, B, Visited) :-
	C1 is C + 1,
	pointsRow(Board, [R, C1], Xs, W1, B1, Aux1),

	(	territory(Board, [R, C], "w", Group, Aux1), length(Group, W2), W is W1 + W2, B is B1 ; 
		territory(Board, [R, C], "b", Group, Aux1), length(Group, B2), B is B1 + B2, W is W1 ),

	append(Aux1, Group, Visited).

% Solo queda el caso que [R, C] es un "-" y no es parte de un territorio,
% luego seguimos iterando
pointsRow(Board, [R, C], ["-" | Xs], W, B, [[R, C] | Visited]) :-
	C1 is C + 1,
	pointsRow(Board, [R, C1], Xs, W, B, Visited).
	% not(territory(Board, [R, C], "w", Group, [])),
	% append(Aux1, Group, Visited).

% Predicado que determina si una posicion [R, C] forma
% parte de un territorio de un color dado, si es asi, guarda en Group las posiciones de 
% dicho territorio
% [R, C] es parte del grupo

%Caso no tenemos mas para recorrer
territory(Board, [R, C], Color, [[R, C]], Visited) :-
	getValueS(Board, [R, C], Value),
	Value = "-",
	% Chequear para cada adyacente de [R, C] si cumplen con territory
	% Obtenemos los adyacentes
	findall(X, adjacent([R, C], X), Adjacents),
	% Es un caso base cuando no tenemos mas opciones que recorrer 
	findall(X, (
				member(X, Adjacents),
				getValueS(Board, X, ValueX),
				ValueX \= Color,
				not(member(X, Visited))
			   ),
			ToVisit),
	ToVisit = [].

% Caso tenemos mas para recorrer
territory(Board, [R, C], Color, [[R, C] | Group], Visited) :-
	getValueS(Board, [R, C], Value),
	Value = "-",
	% Obtenemos los adyacentes
	findall(X, adjacent([R, C], X), Adjacents),
	
	% Chequear para cada adyacente de [R, C] si cumplen con territory
	% Es un caso base cuando no tenemos mas opciones que recorrer 
	findall(X, (
				member(X, Adjacents),
				getValueS(Board, X, ValueX),
				ValueX \= Color,
				not(member(X, Visited))
			   ),
			ToVisit),
	append([[R, C] | ToVisit], Visited, ToVisit1),
	territoryForAdjacents(Board, Color, ToVisit, Group, ToVisit1).

% [R, C] es parte de un borde de color
territory(Board, [R, C], Color, _Group, Visited) :-
	Visited \= [],
	getValueS(Board, [R, C], Value),
	Value = Color.

territoryForAdjacents(_Board, _Color, [], [], _).

% no visitamos la posicion, luego chequeamos si cumple
territoryForAdjacents(Board, Color, [Adj | Adjs], Group, Visited) :-
	territoryForAdjacents(Board, Color, Adjs, GroupAux, Visited),
	append(GroupAux, Visited, Visited1),
	territory(Board, Adj, Color, GroupAux1, Visited1),
	append(GroupAux, GroupAux1, Group).
