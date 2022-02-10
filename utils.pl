:- use_module(dynamic).


% total_chips(N) :- triunfa si N es el total de fichas que hay en el tablero.
total_chips(N) :- findall(_, chip(_, _, _, _, _), L), length(L, N).


% total_player(P, T, N) :- triunfa si N es la cantidad de fichas que hay de tipo T del jugador P. 
total_player(P, T, N) :- T == mo, transformation(P, _), N is 1, !.
total_player(P, T, N) :- T == bt, !, findall(_, (chip(P, T, _, _, K), not(transformation(P, K))), L), length(L, N).
total_player(P, T, N) :- findall(_, (chip(P, T, _, _, _)), L), length(L, N).


% total_in_position(C, R, N) :- triunfa si N es la cantidad de fichas que hay en la posición (C, R). 
total_in_position(C, R, N) :- findall(_, chip(_, _, C, R, _), L), length(L, N).


% is_not_empty(C, R) :- triunfa si hay alguna ficha en la posición (C,R).
is_not_empty(C, R) :- chip(_, _, C, R, _).


% is_empty(C, R) :- triunfa si no hay ninguna ficha en la posición (C,R).
is_empty(C, R) :- not(is_not_empty(C, R)).


% is_even(C) :- triunfa si C es par.
is_even(C) :- R is C mod 2, R == 0.


% is_even(C) :- triunfa si C es impar.
is_odd(C) :- R is C mod 2, R == 1.


% adjacents(C, R, C1, R1, D) :- triunfa si (C,R) y (C1,R1) son posiciones adyacentes con dirección D. 
adjacents(C, R, C1, R1, D) :- is_even(C), adjacents_even(C, R, C1, R1, D).
adjacents(C, R, C1, R1, D) :- is_odd(C), adjacents_odd(C, R, C1, R1, D).

adjacents_even(C, R, C1, R1, D) :- C1 is C - 1, R1 is R - 1, D is 1.
adjacents_even(C, R, C, R1, D) :- R1 is R - 1, D is 2.
adjacents_even(C, R, C1, R1, D) :- C1 is C + 1, R1 is R - 1, D is 3.
adjacents_even(C, R, C1, R, D) :- C1 is C + 1, D is 4.
adjacents_even(C, R, C, R1, D) :- R1 is R + 1, D is 5.
adjacents_even(C, R, C1, R, D) :- C1 is C - 1, D is 6. 

adjacents_odd(C, R, C1, R, D) :- C1 is C - 1, D is 1. 
adjacents_odd(C, R, C, R1, D) :- R1 is R - 1, D is 2.
adjacents_odd(C, R, C1, R, D) :- C1 is C + 1, D is 3.
adjacents_odd(C, R, C1, R1, D) :- C1 is C + 1, R1 is R + 1, D is 4.
adjacents_odd(C, R, C, R1, D) :- R1 is R + 1, D is 5.
adjacents_odd(C, R, C1, R1, D) :- C1 is C - 1, R1 is R + 1, D is 6.


% is_connected(C, R) :- triunfa si la posición (C,R) está conectada al grafo.
is_connected(C, R) :- adjacents(C, R, C1, R1, _), is_not_empty(C1, R1), !.


% graph_is_connected() :- triunfa si la colmena está conectada.
graph_is_connected() :- retractall(mark(_,_,_)), chip(_, _, C, R, _), !, 
                        findall(_, iterate_graph(C, R), _), 
                        findall(_, mark(_, _, _), L1), length(L1, N1),
                        findall([X,Y], chip(_, _, X, Y, _), M),
                        sort(M, L2), length(L2, N2),
                        N1 == N2.

iterate_graph(C, R) :- mark(C, R, _), !.
iterate_graph(C, R) :- not(mark(C, R, _)), assertz(mark(C, R, _)), adjacents(C, R, C1, R1, _), is_not_empty(C1, R1), iterate_graph(C1, R1).


% dont_disconnect_graph(P, T, C, R, K) :- triunfa si la ficha no desconecta el grafo al eliminarla.
dont_disconnect_graph(_, _, C, R, _) :- total_in_position(C, R, N), N > 1.
dont_disconnect_graph(P, T, C, R, K) :- retractall(chip(P, T, C, R, K)), graph_is_connected(), !, assertz(chip(P, T, C, R, K)), !.
dont_disconnect_graph(P, T, C, R, K) :- assertz(chip(P, T, C, R, K)), false.


% same_position(C1, R1, C2, R2) :- triunfa si (C1,R1) es la misma posición que (C2,R2).
same_position(C1, R1, C2, R2) :- C1 == C2, R1 == R2.


% join_lists(L1, L2) :- triunfa si L2 es el resultado de juntar como una sola lista las listas que contiene L1.
join_lists([], []).
join_lists([X|Y], L) :- join_lists(Y, L1), append(X, L1, L). 


% list() :- lista todos los predicados dinámicos.
list() :- listing(game_mode), listing(current_player), listing(chip), 
          listing(above_of), listing(transformation), listing(movement), 
          listing(mark), listing(best_quality), listing(board), 
          listing(drawchip), listing(draw), listing(double).