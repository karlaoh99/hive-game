module(dynamic, [game_mode/1, current_player/1, chip/5, above_of/6, transformation/2, movement/3, mark/3, best_quality/2, board/1, drawchip/5, draw/1, double/1]).
:- import(dynamic).

% game_mode(0): el juego se encuentra en modo jugador contra IA.
% game_mode(1): el juego se encuentra en modo IA contra IA.
:- dynamic game_mode/1.

% current_player(P): el jugador actual es P.
:- dynamic current_player/1.

% chip(P, T, C, R, K): la ficha con id K de tipo T del jugador P, está ubicada en la posición (C,R). 
:- dynamic chip/5.

% above_of(P, T, C, R, K, N): la ficha con id K de tipo T del jugador P, está ubicada en la posición (C,R) encima de otra ficha a un nivel N.
:- dynamic above_of/6.

% transformation(P, K): el mosquito del jugador P con id K está transformado en un escarabajo.
:- dynamic transformation/2.

% movement(P, K, 0): la ficha con identificador K es la última que movió el jugador P.
% movement(P, K, 1): la ficha con identificador K es la última que movió el pillbug del jugador P.
:- dynamic movement/3.

% mark(C, R, D): la posición (C,R) está marcada con una profundidad D durante el recorrido dfs.
:- dynamic mark/3.

% best_quality(Q, L): guarda la mejor calidad Q entre los tableros que se generan en la IA y una lista L con las jugadas que la generan.
:- dynamic best_quality/2.

% board(W): guarda la referencia de la ventana que representa al tablero.
:- dynamic board/1.

% drawchip(C, R, K, H, F) :- la ficha con identificador K que se encuentra en la posición (C,R) está dibujada y H y F son las referencias del hexágono y de la figura respectivamente.
:- dynamic drawchip/5.

% draw(H): H es un hexágono pintado de una posición disponible.
:- dynamic draw/1.

% double(L): guarda una lista L.
:- dynamic double/1.