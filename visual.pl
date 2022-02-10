:- use_module(dynamic).
:- use_module(utils).
:- use_module(place).
:- use_module(move).
:- use_module(ia).

:- use_module(library(pce)).

:- pce_image_directory('./images').
resource(ant, image, image('ant.jpg')).
resource(bee, image, image('bee.jpg')).
resource(beetle, image, image('beetle.jpg')).
resource(ladybug, image, image('ladybug.jpg')).
resource(mosquito, image, image('mosquito.jpg')).
resource(grasshopper, image, image('grasshopper.jpg')).
resource(spider, image, image('spider.jpg')).
resource(pillbug, image, image('pillbug.jpg')).
resource(hive, image, image('hive.jpg')).


choice_image(0, qb, gray, bee).
choice_image(0, bt, gray, beetle).  
choice_image(0, gh, gray, grasshopper).  
choice_image(0, sp, gray, spider).  
choice_image(0, an, gray, ant).  
choice_image(0, lb, gray, ladybug).  
choice_image(0, mo, gray, mosquito).  
choice_image(0, pb, gray, pillbug).  

choice_image(1, qb, black, bee).
choice_image(1, bt, black, beetle).  
choice_image(1, gh, black, grasshopper).  
choice_image(1, sp, black, spider).  
choice_image(1, an, black, ant).  
choice_image(1, lb, black, ladybug).  
choice_image(1, mo, black, mosquito).  
choice_image(1, pb, black, pillbug). 

new_image(Window, Figure, Image, X, Y) :-
    new(Figure, figure),
    new(Bitmap, bitmap(resource(Image), @on)),
    send(Bitmap, name, 1),
    send(Figure, display, Bitmap),
    send(Figure, status, 1),
    send(Window, display, Figure, point(X, Y)).

clear_game() :- retractall(chip(_,_,_,_,_)), 
                retractall(mark(_,_,_)),
                retractall(above_of(_,_,_,_,_,_)), 
                retractall(transformation(_,_)),
                retractall(movement(_,_,_)),
                retractall(draw(_)),
                retractall(drawchip(_,_,_,_,_)),
                retractall(mode_pb()),
                retractall(double(_)),
                retractall(board(_)),
                retractall(current_player(_)),
                retractall(game_mode(_)), 
                retractall(best_quality(_,_)),
                assertz(best_quality(-500,[])).

clear_draw() :- findall(_, (draw(H), send(H, free)), _), retractall(draw(_)).

clear_drawchip() :- findall(_, (drawchip(_, _, _, H, F), send(H, free), send(F, free)), _), retractall(drawchip(_, _, _, _, _)).

clear_chip_image(Ci,Ri, K):- drawchip(Ci,Ri,K,H,F), send(H, free), send(F, free), retractall(drawchip(Ci, Ri, K, H, F)).

start :- 
    clear_game(),
    new(Window, dialog('Hive')),
    new(Board, window('', size(900, 700))),
    assertz(board(Board)),
    assertz(current_player(0)),
    assertz(game_mode(0)),
    new(Chips, dialog_group('')),
    new_image(Chips, F, hive, 0, 0),
    send(F, alignment, center),
    send(Chips, append, new(_, label(nombre, ''))),
    draw_white_panel(WhitePanel, Board),
    draw_black_panel(BlackPanel),
    send(Chips, append, new(_, label(nombre, '     Jugador 1', bold))),
    send(WhitePanel, alignment, center),
    send(Chips, append, WhitePanel),
    send(Chips, append, new(_, label(nombre, '     Jugador 2', bold))),
    send(BlackPanel, alignment, center),
    send(Chips, append, BlackPanel),
    send(Chips, append, new(_, label(nombre, ''))),
    send(Chips, append, new(Op, menu('      Cambiar modo de juego', marked))), 
    send_list(Op, append, ['Jugador contra Inteligencia Artificial', 'Inteligencia Artificial contra Inteligencia Artificial']), 
    send(Op, layout, orientation := vertical),
    send(Chips, append(new(_, button(reiniciar, message(@prolog, restart_button, Op?selection))))),
    send(Chips, append(new(_, button('Jugar IA 1', message(@prolog, game, 0))))),
    send(Chips, append(new(_, button('Jugar IA 2', message(@prolog, game, 1))))),
    send(Window, append, Chips),
    send(Window, append, Board, right),
    send(Window, open).

restart_button(O) :- O == 'Jugador contra Inteligencia Artificial', !, retractall(game_mode(_)), assertz(game_mode(0)), restart.
restart_button(O) :- O == 'Inteligencia Artificial contra Inteligencia Artificial', retractall(game_mode(_)), assertz(game_mode(1)), restart, game(0). 


restart :- 
    retractall(chip(_,_,_,_,_)), 
    retractall(mark(_,_,_)),
    retractall(above_of(_,_,_,_,_,_)), 
    retractall(transformation(_,_)),
    retractall(movement(_,_,_)),
    retractall(mode_pb()),
    retractall(double(_)),
    retractall(current_player(_)),
    assertz(current_player(0)),
    clear_draw(),
    clear_drawchip(),
    retractall(best_quality(_,_)),
    assertz(best_quality(-500,[])).


draw_movement(W, P, T, K, Ci, Ri, Cf, Rf, M) :- clear_draw(), clear_chip_image(Ci, Ri, K), draw_chip(W, P, T, Cf, Rf, M, K).

do_movement(W, P, T, K, Ci,Ri,Cf, Rf) :- move(P,Ci,Ri, Cf,Rf), draw_movement(W, P, T, K, Ci, Ri, Cf, Rf, message(@prolog, generate_movement,W, Cf,Rf,P, T,K)), end_of_turn().



final_pb(W, P, Ci, Ri, C, R) :- retractall(double(_)), clear_draw(), chip(Pa, Ta, Ci, Ri, Ka), assertz(movement(P, Ka, 1)), retractall(chip(_,_,_,_,Ka)), assertz(chip(Pa,Ta,C,R,Ka)),  clear_chip_image(Ci,Ri, Ka), draw_chip(W, Pa, Ta, C, R, message(@prolog, generate_movement,W, C,R,Pa, Ta,Ka), Ka), end_of_turn(). 

draw_movement_adj_pb(W, P, C, R) :- double(L), draw_movement_adj_pb1(W, P, C, R, L).

draw_movement_adj_pb1(_, _, _, _, []).
draw_movement_adj_pb1(W, P, Ci, Ri, [Q|Z]):-
    [C, R] =  Q, 
    hex_to_pixel(C, R, X, Y), 
    draw_empty_hex(W, _, green, X, Y, message(@prolog, final_pb, W, P, Ci, Ri, C, R)),
    draw_movement_adj_pb1(W, P, Ci, Ri, Z). 


draw_possible_movement(_, _, _, _, _, _, []).
draw_possible_movement(W, P, T, K, Ci, Ri, [Q|Z]) :- 
    [C, R] =  Q, 
    hex_to_pixel(C, R, X, Y), 
    draw_empty_hex(W, _ ,green, X, Y, message(@prolog, do_movement, W, P, T, K, Ci, Ri, C, R)),
    draw_possible_movement(W, P, T, K, Ci, Ri,Z).    

draw_possible_lifting(_, _, []).
draw_possible_lifting(W, P, [Q|Z]) :- 
    [C, R] =  Q, 
    hex_to_pixel(C, R, X, Y), 
    draw_empty_hex(W, _ ,red, X, Y, message(@prolog, draw_movement_adj_pb, W, P, C, R)),
    draw_possible_lifting(W, P, Z).    


generate_movement(W, Ci, Ri, P, T, K) :- not(movement(_, K, 1)), clear_draw(), chip(P, qb, _, _, _), is_the_top(Ci, Ri, P, _, _, K), generate_movement1(W,Ci, Ri, P, T, K).
generate_movement1(W,Ci, Ri, P, T, K) :- T==mo, transformation(_,K), !, retractall(chip(P, _, Ci, Ri, K)), graph_is_connected(), !, generate_mv1(Ci, Ri, P, bt, K, L),  assertz(chip(P, bt, Ci, Ri,K)), draw_possible_movement(W, P, T, K, Ci,Ri, L).

generate_movement1(W, C, R, P, T, K) :- 
    T == pb, 
    retractall(double(_)), 
    generate_pillbug_ad(C, R, P, K, A), [X,Y] = A, 
    assertz(double(Y)), 
    draw_possible_lifting(W, P, X), 
    retractall(chip(_, _, _, _, K)), graph_is_connected(), !, 
    generate_mv1(C, R, P, T, K, L), 
    assertz(chip(P, T, C, R, K)), 
    draw_possible_movement(W, P, T, K, C, R, L), !.


generate_movement1(W, Ci, Ri, P, T, K) :- retractall(chip(_, _, _, _, K)), graph_is_connected(), !, generate_mv1(Ci, Ri, P, T, K, L), assertz(chip(P, T, Ci, Ri,K)), draw_possible_movement(W, P, T, K, Ci,Ri, L).
generate_movement1(_, Ci, Ri, P, T, K) :- assertz(chip(P, T, Ci, Ri,K)).

 
draw_placement(W, P, T, C,R) :- place(P, T, C, R, K), clear_draw(), draw_chip(W, P, T, C, R, message(@prolog, generate_movement, W, C, R, P, T, K), K), end_of_turn().

draw_possible_places(_, _, _, []).
draw_possible_places(W, P, T, [Q|Z]) :- 
    [C, R] =  Q, 
    hex_to_pixel(C, R, X, Y), 
    draw_empty_hex(W,_, green, X, Y, message(@prolog, draw_placement, W, P, T, C, R)),
    draw_possible_places(W, P, T, Z).

generate_place(P, T, W) :- clear_draw(), places_available_for_t(P, T, L), draw_possible_places(W,P,T,L).


draw_white_panel(WhitePanel, Board) :- 
    new(WhitePanel, window('', size(275,135))),
    draw_hex(WhitePanel, _ , _, gray, 38, 34, bee, message(@prolog, generate_place, 0, qb, Board)),
    draw_hex(WhitePanel, _, _, gray, 102, 34, beetle, message(@prolog, generate_place, 0, bt, Board)),
    draw_hex(WhitePanel, _,_,  gray, 166, 34, grasshopper, message(@prolog, generate_place, 0, gh, Board)),                  
    draw_hex(WhitePanel, _, _, gray, 230, 34, spider, message(@prolog, generate_place, 0, sp, Board)),                  
    draw_hex(WhitePanel, _, _, gray, 38, 98, ant, message(@prolog, generate_place, 0, an, Board)),                  
    draw_hex(WhitePanel, _, _, gray, 102, 98, ladybug, message(@prolog, generate_place, 0, lb, Board)),  
    draw_hex(WhitePanel, _, _, gray, 166, 98, mosquito, message(@prolog, generate_place, 0, mo, Board)),                  
    draw_hex(WhitePanel, _, _, gray, 230, 98, pillbug, message(@prolog, generate_place, 0, pb, Board)).

draw_black_panel(BlackPanel) :- 
    new(BlackPanel, window('', size(275,135))),
    draw_hex(BlackPanel,_ ,_,black, 38, 34, bee, message(@prolog, do_nothing)),
    draw_hex(BlackPanel,_,_,black, 102, 34, beetle, message(@prolog, do_nothing)),  
    draw_hex(BlackPanel,_,_,black, 166, 34, grasshopper, message(@prolog, do_nothing)),
    draw_hex(BlackPanel,_,_,black, 230, 34, spider, message(@prolog, do_nothing)),    
    draw_hex(BlackPanel,_,_,black, 38, 98, ant, message(@prolog, do_nothing)),
    draw_hex(BlackPanel,_,_,black, 102, 98, ladybug, message(@prolog, do_nothing)),  
    draw_hex(BlackPanel, _,_,black, 166, 98, mosquito, message(@prolog, do_nothing)),       
    draw_hex(BlackPanel,_ ,_,black, 230, 98, pillbug, message(@prolog, do_nothing)).     

draw_chip(W, P, T, C, R, M,K) :- 
    choice_image(P, T, Color, I), 
    hex_to_pixel(C, R, X, Y), 
    draw_hex(W,H,Figure ,Color, X, Y, I, M), 
    assertz(drawchip(C,R,K,H,Figure)).

draw_hex(W, H, Figure,Color, X, Y, F, M) :- 
    C1 is X + 28 * 1,
    C2 is X + 28 * cos(1.046),
    C3 is X + 28 * cos(2.093),
    C4 is X + 28 * cos(3.14),
    C5 is X + 28 * cos(4.186),
    C6 is X + 28 * cos(5.233),
    R1 is Y + 28 * sin(0),
    R2 is Y + 28 * sin(1.046),
    R3 is Y + 28 * sin(2.093),
    R4 is Y + 28 * sin(3.14),
    R5 is Y + 28 * sin(4.186),
    R6 is Y + 28 * sin(5.233),
    draw_lines(W, M, H , [C1,R1], [C2,R2], [C3,R3], [C4,R4], [C5,R5], [C6,R6], Color),
    new_image(W, Figure, F, C5, R5 + 10).


draw_empty_hex(W, H,Color, X, Y, M) :- 
    C1 is X + 28 * 1,
    C2 is X + 28 * cos(1.046),
    C3 is X + 28 * cos(2.093),
    C4 is X + 28 * cos(3.14),
    C5 is X + 28 * cos(4.186),
    C6 is X + 28 * cos(5.233),
    R1 is Y + 28 * sin(0),
    R2 is Y + 28 * sin(1.046),
    R3 is Y + 28 * sin(2.093),
    R4 is Y + 28 * sin(3.14),
    R5 is Y + 28 * sin(4.186),
    R6 is Y + 28 * sin(5.233),
    draw_lines(W, M, H, [C1,R1], [C2,R2], [C3,R3], [C4,R4], [C5,R5], [C6,R6], Color), assertz(draw(H)).
    
draw_lines(W, M ,H, [X1,Y1], [X2,Y2], [X3,Y3], [X4,Y4], [X5,Y5], [X6,Y6], Color) :-
    new(H, path),
    send(H, append, point(X1, Y1)),
    send(H, append, point(X2, Y2)),
    send(H, append, point(X3, Y3)),
    send(H, append, point(X4, Y4)),
    send(H, append, point(X5, Y5)),
    send(H, append, point(X6, Y6)),
    send(H, append, point(X1, Y1)),
    send(H, colour, Color),
    send(H, recogniser, click_gesture(left, '', single, M)),
    send(W, display, H). 

hex_to_pixel(C, R, X, Y) :- is_odd(C), X is (30 * 3 / 2 * C) + 450, Y is (30 * sqrt(3) * (R + 0.5 * 1)) + 350.
hex_to_pixel(C, R, X, Y) :- is_even(C), X is (30 * 3 / 2 * C) + 450, Y is (30 * sqrt(3) * (R + 0.5 * 0)) + 350.    

do_nothing().
draw_ia(W, P, J, L) :- J == 0, !, [T, C, R] = L, chip(P, T, C, R, K), clear_draw(), draw_chip(W, P, T, C, R, message(@prolog, do_nothing), K).
draw_ia(W, P, J, L) :- J == 1, [Ci, Ri, Cf, Rf] = L, !, is_the_top(Cf, Rf, P, T, _, K), draw_movement(W, P, T, K, Ci, Ri, Cf, Rf, message(@prolog, do_nothing)).  


play_ia(P) :- generate_ia(P), best_quality(_, [J, A]), board(W), draw_ia(W, P, J, A), end_of_turn(),  !.

game(P) :- game_mode(1), current_player(P1), P1 == P, play_ia(P), !.


check_can_play1(P, L) :- chip(P, T, Ci, Ri, K), generate_mv(Ci, Ri, P, T, K, L).
check_can_play(P) :- generate_all_places(P, L), length(L, N), N > 0, !.
check_can_play(P) :- findall(M, check_can_play1(P,M), S), join_lists(S, L),  length(L, N) , N > 0 ,!.


change_current_player(P) :- current_player(P1), retractall(current_player(P1)), P is -P1 + 1, assertz(current_player(P)).

is_loser1(Ci, Ri, Cf, Rf) :- adjacents(Ci, Ri, Cf, Rf, _), is_not_empty(Cf, Rf).
is_loser(P, 0) :- not(chip(P, qb, _, _, _)).  
is_loser(P, N) :- chip(P, qb, Ci, Ri, _), findall([Cf,Rf], is_loser1(Ci, Ri, Cf, Rf), L), sort(L, S), length(S, N).  

is_game_over(P) :- is_loser(P, N), N == 6, !.

string_game_over(P, S) :- P == 0, !, S = 'Usted ha perdido.'.
string_game_over(P, S) :- P == 1, !, S = 'Usted ha ganado.'. 

end_of_turn() :- 
    is_game_over(P), !,
    string_game_over(P, S), 
    new(D, dialog('')), 
    new(L, label(game_over, S)),
    new(B, button('Aceptar', and(message(@prolog, restart), message(D, destroy), message(D, free)))), 
    send(D, append(L)), 
    send(D, append(B)), 
    send(D, open), false.
end_of_turn() :-
    game_mode(0),
    change_current_player(P),
    check_can_play(P),
    P == 1, play_ia(P), !. 
end_of_turn() :-
    game_mode(0),
    current_player(P),
    not(check_can_play(P)),
    P == 0, play_ia(1), !.
end_of_turn():- 
    game_mode(1), 
    change_current_player(P),
    not(check_can_play(P)), 
    change_current_player(_), !. 
end_of_turn() :- true.

