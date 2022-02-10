:- use_module(dynamic).
:- use_module(utils).


is_the_top(C, R, P, T, N, K) :- total_in_position(C, R, N), N == 1, !, chip(P, T, C, R, K), !.
is_the_top(C, R, P, T, N, K) :- total_in_position(C, R, N), above_of(P, T,  C, R, K, N).


add_above_of(C, R,_,_, _) :- is_empty(C, R), !.
add_above_of(C, R, P, T, K) :- total_in_position(C, R, N), N1 is N + 1, assertz(above_of(P, T, C, R, K, N1)), !.

delete_above_of(C, R) :- total_in_position(C, R, N), N == 0, !.
delete_above_of(C, R) :- total_in_position(C, R, N), N1 is N + 1, retractall(above_of(_, _,  C, R, _, N1)).

verify_mosquito(C, R,P, K) :- is_empty(C, R), transformation(P, K), !, assertz(chip(P, mo, C, R, K)), retractall(transformation(P, K)).
verify_mosquito(C, R, P, K) :- assertz(chip(P, bt, C, R, K)).

add_chip(Ci, Ri, Cf, Rf, P, T, K) :- T == bt, !, retractall(chip(_,_,_,_,K)), add_above_of(Cf, Rf, P, bt, K), delete_above_of(Ci, Ri), verify_mosquito(Cf, Rf, P, K). 
add_chip(Ci, Ri, Cf, Rf, P, T, K) :- T == mo, is_not_empty(Cf, Rf), assertz(transformation(P, K)), add_chip(Ci, Ri, Cf, Rf, P, bt, K).
add_chip(_, _, Cf, Rf, P, T, K) :- retractall(chip(_,_,_,_,K)), assertz(chip(P, T, Cf, Rf, K)). 

is_straight(Ci, Ri, Cf, Rf, _) :- is_empty(Ci, Ri), Cf is Ci, Rf is Ri,!.
is_straight(Ci, Ri, Cf, Rf, D) :- D == 1, is_even(Ci), !, C1 is Ci - 1, R1 is Ri - 1, is_straight(C1, R1, Cf, Rf, D). 
is_straight(Ci, Ri, Cf, Rf, D) :- D == 2, is_even(Ci), !, C1 is Ci, R1 is Ri - 1, is_straight(C1, R1, Cf, Rf, D).
is_straight(Ci, Ri, Cf, Rf, D) :- D == 3, is_even(Ci), !, C1 is Ci + 1, R1 is Ri - 1, is_straight(C1, R1, Cf, Rf, D).
is_straight(Ci, Ri, Cf, Rf, D) :- D == 4, is_even(Ci), !, C1 is Ci + 1, R1 is Ri, is_straight(C1, R1, Cf, Rf, D).
is_straight(Ci, Ri, Cf, Rf, D) :- D == 5, is_even(Ci), !, C1 is Ci, R1 is Ri + 1, is_straight(C1, R1, Cf, Rf, D).
is_straight(Ci, Ri, Cf, Rf, D) :- D == 6, is_even(Ci), !, C1 is Ci - 1, R1 is Ri, is_straight(C1, R1, Cf, Rf, D).
is_straight(Ci, Ri, Cf, Rf, D) :- D == 1, is_odd(Ci), !, C1 is Ci - 1, R1 is Ri, is_straight(C1, R1, Cf, Rf, D). 
is_straight(Ci, Ri, Cf, Rf, D) :- D == 2, is_odd(Ci), !, C1 is Ci, R1 is Ri - 1, is_straight(C1, R1, Cf, Rf, D).
is_straight(Ci, Ri, Cf, Rf, D) :- D == 3, is_odd(Ci), !, C1 is Ci + 1, R1 is Ri, is_straight(C1, R1, Cf, Rf, D).
is_straight(Ci, Ri, Cf, Rf, D) :- D == 4, is_odd(Ci), !, C1 is Ci + 1, R1 is Ri + 1, is_straight(C1, R1, Cf, Rf, D).
is_straight(Ci, Ri, Cf, Rf, D) :- D == 5, is_odd(Ci), !, C1 is Ci, R1 is Ri + 1, is_straight(C1, R1, Cf, Rf, D).
is_straight(Ci, Ri, Cf, Rf, D) :- D == 6, is_odd(Ci), !, C1 is Ci - 1, R1 is Ri + 1, is_straight(C1, R1, Cf, Rf, D).

generate_displacements(C, R, D) :- mark(C,R,D1) ,  D < D1, !,retract(mark(C, R,_)), assertz(mark(C, R,D)), adjacents(C, R, C1, R1, Dir), is_not_blocked(C, R, Dir), mark(C1, R1, _), D2 is D + 1 ,generate_displacements(C1, R1, D2).
generate_displacements(C, R, _) :- mark(C,R,_), !.
generate_displacements(C, R, D) :- not(mark(C, R,_)),! , assertz(mark(C, R,D)), adjacents(C, R, C1, R1, Dir), is_empty(C1,R1), is_connected(C1, R1), is_not_blocked(C, R, Dir), D1 is D + 1, generate_displacements(C1, R1,D1).

transformation_mosquito(Ci,Ri,P,T,K ,L):- generate_mv(Ci,Ri,P,T,K,L).

is_blocked_even(C, R, D) :- D == 1, C1 is C - 1, R1 is R, C2 is C, R2 is R - 1, is_not_empty(C1, R1), is_not_empty(C2, R2).
is_blocked_even(C, R, D) :- D == 2, C1 is C - 1, R1 is R - 1, C2 is C + 1, R2 is R - 1, is_not_empty(C1, R1), is_not_empty(C2, R2). 
is_blocked_even(C, R, D) :- D == 3, C1 is C, R1 is R - 1, C2 is C + 1, R2 is R, is_not_empty(C1, R1), is_not_empty(C2, R2). 
is_blocked_even(C, R, D) :- D == 4, C1 is C + 1, R1 is R - 1, C2 is C, R2 is R + 1, is_not_empty(C1, R1), is_not_empty(C2, R2). 
is_blocked_even(C, R, D) :- D == 5, C1 is C + 1, R1 is R, C2 is C - 1, R2 is R, is_not_empty(C1, R1), is_not_empty(C2, R2). 
is_blocked_even(C, R, D) :- D == 6, C1 is C, R1 is R + 1, C2 is C - 1, R2 is R - 1, is_not_empty(C1, R1), is_not_empty(C2, R2). 

is_blocked_odd(C, R, D) :- D == 1, C1 is C - 1, R1 is R + 1, C2 is C, R2 is R - 1, is_not_empty(C1, R1), is_not_empty(C2, R2). 
is_blocked_odd(C, R, D) :- D == 2, C1 is C - 1, R1 is R, C2 is C + 1, R2 is R, is_not_empty(C1, R1), is_not_empty(C2, R2). 
is_blocked_odd(C, R, D) :- D == 3, C1 is C, R1 is R - 1, C2 is C + 1, R2 is R + 1, is_not_empty(C1, R1), is_not_empty(C2, R2). 
is_blocked_odd(C, R, D) :- D == 4, C1 is C + 1, R1 is R, C2 is C, R2 is R + 1, is_not_empty(C1, R1), is_not_empty(C2, R2). 
is_blocked_odd(C, R, D) :- D == 5, C1 is C + 1, R1 is R + 1, C2 is C - 1, R2 is R + 1, is_not_empty(C1, R1), is_not_empty(C2, R2). 
is_blocked_odd(C, R, D) :- D == 6, C1 is C, R1 is R + 1, C2 is C - 1, R2 is R, is_not_empty(C1, R1), is_not_empty(C2, R2).  

is_not_blocked(C, R, D) :- is_even(C), not(is_blocked_even(C, R, D)).
is_not_blocked(C, R, D) :- is_odd(C), not(is_blocked_odd(C, R, D)).

is_stacked_hollow(Ci,Ri):-   findall([Cf,Rf], (adjacents(Ci,Ri,Cf,Rf,_), total_in_position(Cf,Rf,N), N > 1), L), length(L,M) , M==6. 


generate_queen_mv(Ci, Ri, Cf, Rf) :- adjacents(Ci, Ri, Cf, Rf, D), is_empty(Cf, Rf), is_connected(Cf, Rf), is_not_blocked(Ci, Ri, D).

generate_beetle_mv(Ci, Ri, Cf, Rf):- adjacents(Ci, Ri, Cf, Rf, _), is_connected(Cf, Rf).

move_grasshopper1(Ci, Ri, Cf, Rf) :- is_straight(Ci, Ri, Cf, Rf, 1).
move_grasshopper1(Ci, Ri, Cf, Rf) :- is_straight(Ci, Ri, Cf, Rf, 2).
move_grasshopper1(Ci, Ri, Cf, Rf) :- is_straight(Ci, Ri, Cf, Rf, 3).
move_grasshopper1(Ci, Ri, Cf, Rf) :- is_straight(Ci, Ri, Cf, Rf, 4).
move_grasshopper1(Ci, Ri, Cf, Rf) :- is_straight(Ci, Ri, Cf, Rf, 5).
move_grasshopper1(Ci, Ri, Cf, Rf) :- is_straight(Ci, Ri, Cf, Rf, 6).

generate_grasshopper_mv(Ci, Ri, Cf, Rf) :-  move_grasshopper1(Ci, Ri, Cf, Rf), not(adjacents(Ci, Ri, Cf, Rf, _)), is_empty(Cf,Rf).


generate_ant_mv(Ci, Ri, Cf, Rf) :- retractall(mark(_, _,_)), findall(_, generate_displacements(Ci,Ri,0), _), retract(mark(Ci, Ri,_)), mark(Cf, Rf,_).

generate_spider_mv(Ci, Ri, Cf, Rf) :- retractall(mark(_, _,_)), findall(_, generate_displacements(Ci,Ri,0), _), retract(mark(Ci, Ri,_)), mark(Cf, Rf,3).

move_lady(Ci, Ri, Cf, Rf ,D) :- D == 3 ,!,Cf is Ci, Rf is Ri.
move_lady(Ci, Ri,Cf,Rf, D) :- is_not_empty(Ci,Ri),! ,adjacents(Ci, Ri, C1, R1, _) ,D1 is D + 1, move_lady(C1,R1, Cf, Rf, D1).

generate_ladybug_mv(Ci,Ri,Cf,Rf) :- adjacents(Ci, Ri, C, R, _), move_lady(C ,R , Cf , Rf , 1) ,is_empty(Cf,Rf), not(same_position(Ci, Ri, Cf, Rf)),is_connected(Cf,Rf).

is_valid_mos_adjacent(C, R) :- is_empty(C, R), !.
is_valid_mos_adjacent(C, R) :-  is_the_top(C, R, _, T, _, _), !, T \= mo.

not_mosquito_adjacent(C, R) :- findall(_, (adjacents(C, R, Ca, Ra, _), is_valid_mos_adjacent(Ca, Ra)), L), length(L, N), N == 6, !.

get_top_adyacents(C, R, Ca, Ra, Pa, Ta, Ka) :- adjacents(C, R, Ca, Ra, _), is_not_empty(Ca, Ra), is_the_top(Ca, Ra, Pa, Ta, _, Ka). 

generate_mosquito_mv(Ci, Ri, P, K,L):- not_mosquito_adjacent(Ci, Ri), get_top_adyacents(Ci,Ri,_,_,_,Ta,_), generate_mosquito_mv1(Ci, Ri, P, K,L, Ta) .
generate_mosquito_mv1(Ci, Ri, P, K,L, Ta):- assertz(chip(P, Ta, Ci, Ri,K)), transformation_mosquito(Ci,Ri,P,Ta,K ,L), retractall(chip(P, Ta, Ci, Ri,K)), ! .
generate_mosquito_mv1(Ci, Ri, P, K,_, Ta):-  retractall(chip(P, Ta, Ci, Ri,K)), ! .


verify2(C, R, Ca, Ra, Pa, Ta, Ka) :- get_top_adyacents(C, R, Ca, Ra, Pa, Ta, Ka), total_in_position(Ca, Ra, N), N == 1. 

verify3(P, Pa, Ka):- Pa \= P , !,not(movement(Pa,Ka,_)).  
verify3(_, _, Ka):- not(movement(_,Ka,1)).


generate_pillbug_mv2(P, Ci, Ri, _, Ca, Ra, Cm, Rm) :- verify2(Ci,Ri,C,R,Pa,Ta,Ka), verify3(P,Pa,Ka) , dont_disconnect_graph(Pa, Ta, C,R, Ka) , adjacents(Ci ,Ri, Cv, Rv, _) ,  is_empty(Cv,Rv) , not(is_stacked_hollow(Cv,Rv)), Ca is C,Ra is R , Cm is Cv, Rm is Rv.

generate_pillbug_mv1(P,Ci, Ri, K, L):- findall( [Ca,Ra,Cm,Rm], generate_pillbug_mv2(P,Ci  ,Ri, K, Ca , Ra ,Cm,Rm), L1), update_list(L1,A1,F1),sort(A1, A),sort(F1, F), L = [A,F].


update_list([],[],[]).
update_list([X|Y],A,F):- [Ca,Ra, Cm,Rm] =  X , update_list(Y, A1,F1), append([[Ca,Ra]], A1,A), append([[Cm,Rm]], F1,F). 

generate_pillbug_mv(Ci, Ri, _ , _ ,L) :- generate_mv1(Ci, Ri, _, qb, _, L).
generate_pillbug_ad(Ci, Ri, P, K, A) :- generate_pillbug_mv1(P, Ci, Ri, K, A).

generate_mv1(Ci, Ri,_, T, _, L):- T == qb, !, findall([Cf,Rf], generate_queen_mv(Ci, Ri, Cf, Rf), L). 
generate_mv1(Ci, Ri,_, T,_, L) :- T == bt, !, findall([Cf,Rf], generate_beetle_mv(Ci, Ri, Cf, Rf), L).
generate_mv1(Ci, Ri, P, T, K, L) :- T == gh,!, assertz(chip(P,T,Ci,Ri, K)), findall([Cf,Rf], generate_grasshopper_mv(Ci, Ri, Cf, Rf), L), retractall(chip(P,T,Ci,Ri, K)) .
generate_mv1(Ci, Ri,_, T,_, L) :- T == an , !, findall([Cf,Rf], generate_ant_mv(Ci,Ri,Cf,Rf),L). 
generate_mv1(Ci, Ri,_, T,_, L) :- T == sp , !, findall([Cf,Rf], generate_spider_mv(Ci, Ri, Cf, Rf ), L). 
generate_mv1(Ci, Ri,_, T,_, L) :- T == lb , !,findall( [Cf,Rf], generate_ladybug_mv(Ci , Ri, Cf, Rf),L). 
generate_mv1(Ci, Ri,P, T,K, L) :- T == mo, !, findall( M,generate_mosquito_mv(Ci, Ri,P,K,M),L1), join_lists(L1, L). 
generate_mv1(Ci, Ri,P, T,K, L) :- T == pb, !, generate_pillbug_mv(Ci, Ri,P ,K, L). 

generate_mv2(Ci, Ri, P, T, K,L) :- retractall(chip(P, T, Ci, Ri, K)), graph_is_connected(),!,generate_mv1(Ci, Ri, P, T, K, L), assertz(chip(P, T, Ci, Ri,K)), !, length(L, N), N > 0.
generate_mv2(Ci, Ri, P, T, K,_) :-  assertz(chip(P, T, Ci, Ri,K)), false.

generate_mv(Ci, Ri, P, T, K,L) :- chip(P, qb, _, _, _), is_the_top(Ci, Ri, _, T, _, K), generate_mv2(Ci, Ri, P, T, K,L). 


move(P, Ci, Ri, Cf, Rf) :- is_the_top(Ci, Ri, _, T, _, K), add_chip(Ci, Ri, Cf, Rf, P, T, K), update_movement(P,K,0), P1 is -P + 1, retractall(movement(P1,_,1)), !.


update_movement(P,K,I) :- retractall(movement(P,_,_)), assertz(movement(P,K,I)).
