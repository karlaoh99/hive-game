:- use_module(dynamic).
:- use_module(utils).


% valid_adjacent(P, C, R) :- triunfa si la posición (C,R) está vacía o la ficha que se encuentra en la cima de esa posición pertenece al jugador P.
valid_adjacent(_, C, R) :- is_empty(C, R), !.
valid_adjacent(P, C, R) :- findall(_, chip(_, _, C, R, _), L), length(L, N), N == 1, !, chip(P, _, C, R, _).
valid_adjacent(P, C, R) :- findall(_, chip(_, _, C, R, _), L), length(L, N), above_of(P, _, C, R, _, N).


% same_player(P, C, R) :- triunfa si alrededor de la posición (C,R) todas las fichas pertenecen al jugador P o están vacías.
same_player(P, C, R) :- is_odd(C), !,
                        C1 is C + 1, C2 is C - 1, R1 is R + 1, R2 is R - 1,
                        valid_adjacent(P, C2, R),
                        valid_adjacent(P, C, R2),
                        valid_adjacent(P, C1, R),
                        valid_adjacent(P, C1, R1),
                        valid_adjacent(P, C, R1),
                        valid_adjacent(P, C2, R1).
same_player(P, C, R) :- is_even(C),
                        C1 is C + 1, C2 is C - 1, R1 is R + 1, R2 is R - 1,
                        valid_adjacent(P, C2, R2),
                        valid_adjacent(P, C, R2),
                        valid_adjacent(P, C1, R2),
                        valid_adjacent(P, C1, R),
                        valid_adjacent(P, C, R1),
                        valid_adjacent(P, C2, R).


% valid_chip(P, T) :- triunfa si se puede añadir otra ficha de tipo T del jugador P.
valid_chip(P, T) :- total_player(P, T, N), N == 0, !.
valid_chip(P, T) :- T == bt, !, total_player(P, T, N), N < 2.
valid_chip(P, T) :- T == gh, !, total_player(P, T, N), N < 3.
valid_chip(P, T) :- T == sp, !, total_player(P, T, N), N < 2.
valid_chip(P, T) :- T == an, !, total_player(P, T, N), N < 3.


% valid_queen(P, T) :- triunfa si el jugador P va a colocar a la reina, ya la colocó o la cantidad de fichas que ha puesto es menor que 3.
valid_queen(_, T) :- T == qb, !.
valid_queen(P, _) :- chip(P, qb, _, _, _), !.
valid_queen(P, _) :- total_player(P, _, N), N < 3.
 

% can_place(P, T) :- triunfa si el jugador P puede escoger una ficha de tipo T para ubicar en el tablero. 
can_place(P, T) :- valid_chip(P, T), valid_queen(P, T).


% places_available(P, L) :- triunfa si L son las posiciones disponibles en las que el jugador P podría colocar una ficha.
places_available(_, L) :- not(chip(_, _, _, _, _)), !, L = [[0,0]].
places_available(P, L) :- not(chip(P, _, _, _, _)), !, Pa is - P + 1, chip(Pa, _, Ci, Ri, _), findall([Cf, Rf], (adjacents(Ci, Ri, Cf, Rf, _), is_empty(Cf, Rf)), L), !.
places_available(P, L) :- findall([Cf, Rf], (chip(P, _, Ci, Ri, _), adjacents(Ci, Ri, Cf, Rf, _), is_empty(Cf, Rf), same_player(P, Cf, Rf)), L).


% places_available(P, L) :- triunfa si L son las posiciones disponibles en las que el jugador P puede colocar una ficha de tipo T.
places_available_for_t(P, T, L) :- can_place(P, T), places_available(P, L).


% place(P, T, C, R, K) :- triunfa si el jugador P puede colocar una ficha de tipo T en la posición (C,R) y K es el identificador de la ficha.
place(P, T, C, R, K) :- total_chips(K1), K is K1 + 1, assertz(chip(P, T, C, R, K)), retractall(movement(_,_,1)).