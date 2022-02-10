:- use_module(dynamic).
:- use_module(utils).
:- use_module(place).
:- use_module(move).


generate_ia(P) :- retractall(best_quality(_,_)), 
                  assertz(best_quality(-500,[])), 
                  generate_all_places(P, L1), 
                  for_list_place(P, L1), 
                  generate_all_mov(P, L2),
                  for_list_move(P, L2),
                  generate_all_pb_lifting(P, L3),
                  for_list_lifting_pb(P, L3),
                  best_quality(Q, L), 
                  length(L, N),
                  N > 0,
                  random(0, N, R),
                  nth0(R, L, E),
                  [G, H] = E,  
                  execute(P, G, H),
                  retractall(best_quality(_,_)),
                  assertz(best_quality(Q, [G, H])), !.  


generate_all_places1(P, T) :- T = qb, can_place(P, T).
generate_all_places1(P, T) :- T = bt, can_place(P, T).
generate_all_places1(P, T) :- T = gh, can_place(P, T).
generate_all_places1(P, T) :- T = mo, can_place(P, T).
generate_all_places1(P, T) :- T = pb, can_place(P, T).
generate_all_places1(P, T) :- T = sp, can_place(P, T).
generate_all_places1(P, T) :- T = an, can_place(P, T).
generate_all_places1(P, T) :- T = lb, can_place(P, T).

generate_all_places(P, L) :- places_available(P, M), length(M, N), N > 0, findall([T, M], generate_all_places1(P, T), L), !.
generate_all_places(_, []).


generate_all_mov(P, L) :- findall([C, R, M], (chip(P, T, C, R, K), generate_mv(C, R, P, T, K, M)), L).


generate_all_pb_lifting(_, []). 
generate_all_pb_lifting(P, L) :- chip(P, pb, C, R, K), generate_pillbug_ad(C, R, P, K, L), !.


for_list_place(_, []).
for_list_place(P, [X|Y]) :- [T, M] = X , for_list_place1(P, T, M), for_list_place(P, Y).

for_list_place1(_, _, []).
for_list_place1(P, T, [X|Y]) :- [C, R] = X, analyze_place(P, T, C, R), for_list_place1(P, T, Y).


for_list_move(_, []).
for_list_move(P, [X|Y]) :- [Ci, Ri, M] = X, 
                           for_list_move_1(P, Ci, Ri, M), 
                           for_list_move(P, Y).

for_list_move_1(_, _, _, []).
for_list_move_1(P, Ci, Ri, [X|Y]) :- [Cf, Rf] = X, analyze_move(P, Ci, Ri, Cf, Rf), for_list_move_1(P, Ci, Ri, Y).


for_list_lifting_pb(_, []).
for_list_lifting_pb(P, L) :- [A, F] = L, for_list_lifting_pb1(P, F, A).

for_list_lifting_pb1(_, _, []).
for_list_lifting_pb1(P, F, [X|Y]) :- [C, R] = X, for_list_move_pb(P, C, R, F), for_list_lifting_pb1(P, F, Y).

for_list_move_pb(_, _, _, []). 
for_list_move_pb(P, Ci, Ri, [X|Y]) :- [Cf, Rf] = X, analyze_move_pb(Ci, Ri, Cf, Rf), for_list_move_pb(P, Ci, Ri, Y), !.


analyze_place(P, T, C, R) :- total_chips(K1), K is K1 + 1, 
                             assertz(chip(P, T, C, R, K)), 
                             board_state(P, Q), 
                             L = [T, C, R], 
                             update_quality(Q, 0, L), 
                             retractall(chip(P, T, C, R, K)).  


analyze_move(P, Ci, Ri, Cf, Rf) :- move_ia(P, Ci, Ri, Cf, Rf), 
                                   board_state(P, Q), 
                                   L = [Ci, Ri, Cf, Rf], 
                                   update_quality(Q, 1, L), 
                                   move_ia(P, Cf, Rf, Ci, Ri).


analyze_move_pb(Ci, Ri, Cf, Rf) :- chip(P, _, Ci, Ri, _), analyze_move(P, Ci, Ri, Cf, Rf).


move_ia(P, Ci, Ri, Cf, Rf) :- is_the_top(Ci, Ri, _, T, _, K), add_chip(Ci, Ri, Cf, Rf, P, T, K).


execute(P, J, L) :- J == 0, !, [T, C, R] = L, place(P, T, C, R, _), !.
execute(P, J, L) :- J == 1, !, [Ci, Ri, Cf, Rf] = L, is_the_top(Ci, Ri, P, _, _, _),  move(P, Ci, Ri, Cf, Rf). 


board_state(P, Q) :- 
    Pa is -P + 1, 
    covered_queen(Pa, Q1),
    locked_queen(P, Q2), 
    locked_queen(Pa, Q3), 
    blocked_ants(Pa, Q4),
    count_chips_not_move(Pa, Q5),
    count_chips_not_move(P, Q6),
    to_win(P, Q7), 
    to_lose(P, Q8),
    chips_out_and_board(P, Q9, Q10),
    Q is (Q1 + (Q3 - Q2) * 10 + Q4 + (Q5 - Q6) + Q7 + Q8) + (Q10 - Q9), !.


sum(K, K1, Q) :- K \= K1, !, Q = 20. 
sum(_, _, Q) :- Q = 0. 
covered_queen(Pa, Q) :- not(chip(Pa, qb, _, _, _)), Q = 0.
covered_queen(Pa, Q) :- chip(Pa, qb, C, R, K), is_the_top(C, R, _, _, _, K1), sum(K, K1, Q).

locked_queen(P, Q) :- is_loser(P, N), Q is N.

to_win(P, Q) :- P1 is -P + 1, is_loser(P1, N), N == 6, Q = 150.
to_win(_, Q) :- Q = 0.

to_lose(P, Q) :- is_loser(P, N), N == 6, Q = -150.
to_lose(_, Q) :- Q = 0.

chips_out_and_board(P, Out, Board):- total_player(P, _, Board), Out is 14 - Board.  

ant_is_blocked(C, R) :- 
    is_even(C), 
    is_blocked_even(C, R, 1), 
    is_blocked_even(C, R, 2), 
    is_blocked_even(C, R, 3),  
    is_blocked_even(C, R, 4),
    is_blocked_even(C, R, 5),
    is_blocked_even(C, R, 6).

ant_is_blocked(C, R) :- 
    is_odd(C), 
    is_blocked_odd(C, R, 1), 
    is_blocked_odd(C, R, 2), 
    is_blocked_odd(C, R, 3),  
    is_blocked_odd(C, R, 4),
    is_blocked_odd(C, R, 5),
    is_blocked_odd(C, R, 6).

blocked_ants(P, Q) :- findall([C, R, K], (chip(P, an, C, R, K), ant_is_blocked(C, R)), L), length(L, Q).

count_chips_not_move(P, Q) :- can_move(P, N), !, chips_out_and_board(P, _, Board), Q is Board - N .
count_chips_not_move(P, Q) :- chips_out_and_board(P, _, Board), Q = Board.

can_move1(P, L) :- chip(P, T, Ci, Ri, K), generate_mv(Ci, Ri, P, T, K, L).
can_move(P, N) :- findall(M, (can_move1(P ,M), M \= []), S), length(S, N), !.


update_quality(Q, J, L) :- best_quality(Q1, L1), Q == Q1, append([[J, L]], L1, L2), retractall(best_quality(_,_)), assertz(best_quality(Q, L2)), !.
update_quality(Q, J, L) :- best_quality(Q1, _), Q > Q1, retractall(best_quality(_,_)), assertz(best_quality(Q, [[J, L]])), !.
update_quality(_, _, _).