:- dynamic(exit_locations/1).
:- dynamic(giant_locations/1).
:- dynamic(isGiant/1).
:- dynamic(isFood/1).
:- dynamic(isFlashlight/1).
:- dynamic(isWeapon/1).
:- dynamic(isLight/1).
:- dynamic(isExit/1).
:- dynamic(power/1).
:- dynamic(capacity/1).
:- dynamic(world_rows/1).
:- dynamic(world_cols/1).
:- dynamic(food_locations/1).
:- dynamic(flashlight_locations/1).
:- dynamic(light_locations/1).
:- dynamic(weapon_locations/1).
:- dynamic(visited/1).
:- dynamic(visited_cells/1).
:- dynamic(step/1).
:- dynamic(map_food_locations/1).
:- dynamic(actions/1).


step(1).

start :- 
    write('Game is started! Agent should do best actions!'), nl,
    write('**********************************************'), nl,
    init_game,
    init_map,
    init_agent,
    stage.
    
stage :-
    step(S),
    format('Step ~p:', [S]),
    Next_s is S+1,
    retract(step(S)),
    asserta(step(Next_s)),
    agent_location(Agent_Loc),
    format('\nAgent position : ~p~n',[Agent_Loc]),
    power(P),
    write('Current power: '), write(P), nl,
    format('*********************~n'),
    write('Updated map information:'), nl,
    show_map,
    format('*********************~n'),
    write('before perceptions :\n'),
    (leftHand(empty) -> LH is 0,write('his left hand is empty\n');
    (leftHand(weapon) -> LH is 1,write('there is a weapon in his left hand\n');
    LH is 2,write('there is a flashlight in his left hand\n'))),
    (rightHand(empty) -> RH is 0,write('his right hand is empty\n');
    (rightHand(weapon) -> RH is 1,write('there is a weapon in his right hand\n');
    RH is 2,write('there is a flashlight in his right hand\n'))),
    make_percept([Shout,Flashlight,Weapon,Food,Exit,Giant]),
    write('*********************'), nl,
    write('perceptions:\n'),
    (light_location(LL),LL=Agent_Loc->write('there is a light here\n');
    write('there is no light here\n')),
    (Weapon = yes -> write('there is a weapon here\n');
    write('there is no weapon here\n')),
    (Flashlight = yes -> write('there is a flashlight here\n');
    write('there is no flashlight here\n')),
    (Food = yes -> write('there is some food here\n');
    write('there is no food here\n')),
    (Giant = yes -> write('there is a giant here\n');
    write('there is no giant here\n')),
    (Shout = yes -> write('the giant shout can be heard here\n');
    write('there is no shouting sound here\n')),
    (Exit = yes -> write('there is a exist here\n');
    write('there is no exit here\n')),
    visited_cells(VC),
    append([Agent_Loc],VC,NVC),
    retractall(visited_cells(_)),
    asserta(visited_cells(NVC)),
    handle_percept([Shout,Flashlight,Weapon,Food,Exit,Giant]),
    write('*********************'), nl,
    write('after perceptions :\n'),
    (leftHand(empty) -> NLH is 0,write('your left hand is empty\n');
    (leftHand(weapon) -> NLH is 1,write('there is a weapon in your left hand\n');
    NLH is 2,write('there is a flashlight in left hand\n') ) ),
    (rightHand(empty) -> NRH is 0,write('your right hand is empty\n');
    (rightHand(weapon) -> NRH is 1,write('there is a weapon in your right hand\n');
    NRH is 2,write('there is a flashlight in your right hand\n'))),
    actions(A),
    list_rev(A, NA),
    format('Actions sequence: ~p~n',[NA]),
    (Exit=yes -> (answer),write('he succeeded.\n'),true;
    (do_action(Action),format('*********************~nAction is go to ~p~n',[Action]),
    
    stage -> true;
    write('he failed to answer the question.\n'))).

show_map:-
    food_locations(FL),
    format('Food locations: ~p',[FL]), tab(10),
    flashlight_locations(FLL),
    format('Flashlight locations: ~p ~n',[FLL]),
    weapon_locations(WL),
    format('Weapon locations: ~p',[WL]), tab(10),
    giant_locations(GL),
    format('Giant locations: ~p ~n',[GL]).

answer:-
    question(X,L),
    answer(X,L).
    
answer(flashlight,L):- isFlashlight(yes,L).
answer(flashlight,L):- isFlashlight(no,L).

answer(giant,L):- isGiant(yes,L).
answer(giant,L):- isGiant(no,L).

answer(food,L):- isFood(yes,L).
answer(food,L):- isFood(no,L).

answer(exit,L):- isExit(yes,L).
answer(exit,L):- isExit(no,L).

answer(weapon,L):- isWeapon(yes,L).
answer(weapon,L):- isWeapon(no,L).


init_game :-
    retractall( isGiant(_,_) ),
    retractall( isFood(_,_) ),
    retractall( isFlashlight(_,_) ),
    retractall( isWeapon(_,_)),
    retractall( isLight(_,_) ),
    retractall( isExit(_,_) ),
    retractall( visited_cells(_) ),
    asserta( visited_cells([]) ).
    
init_map :-
    retractall( power(_) ),
    retractall( capacity(_)),
    retractall( world_rows(_)),
    retractall( world_cols(_)),
    retractall( food_locations(_)),
    retractall( flashlight_locations(_)),
    retractall( light_locations(_)),
    retractall( weapon_locations(_)),
    retractall( exit_locations(_)),
    retractall( giant_locations(_)),
    consult('data.db'),
    retractall( food_location(_)),
    food_locations(X1),
    insert_food_location(X1),
    retractall( flashlight_location(_)),
    flashlight_locations(X2),
    insert_flashlight_location(X2),
    retractall( light_location(_)),
    light_locations(X3),
    insert_light_location(X3),
    retractall( weapon_location(_)),
    weapon_locations(X4),
    insert_weapon_location(X4),
    retractall( exit_location(_)),
    exit_locations(X5),
    insert_exit_location(X5),
    retractall( giant_location(_)),
    giant_locations(X6),
    insert_giant_location(X6).

%add food locations to food_location
insert_food_location(Z):-
    Z = [],!.
insert_food_location(Z):-
    Z = [X|Y],
    asserta(food_location(X)),
    insert_food_location(Y).

%add flashlight locations to flashlight_location
insert_flashlight_location(Z):-
    Z = [],!.
insert_flashlight_location(Z):-
    Z = [X|Y],
    asserta(flashlight_location(X)),
    insert_flashlight_location(Y).
    
%add light locations to light_location
insert_light_location(Z):-
    Z = [],!.
insert_light_location(Z):-
    Z = [X|Y],
    asserta(light_location(X)),
    insert_light_location(Y).
    
%add weapon locations to weapon_location
insert_weapon_location(Z):-
    Z = [],!.
insert_weapon_location(Z):-
    Z = [X|Y],
    asserta(weapon_location(X)),
    insert_weapon_location(Y).
    
%add exit locations to exit_location
insert_exit_location(Z):-
    Z = [],!.
insert_exit_location(Z):-
    Z = [X|Y],
    asserta(exit_location(X)),
    insert_exit_location(Y).
    
%add giant locations to giant_location
insert_giant_location(Z):-
    Z = [],!.
insert_giant_location(Z):-
    Z = [X|Y],
    asserta(giant_location(X)),
    insert_giant_location(Y).
    
init_agent :-
    
    retractall( leftHand(_)),
    retractall( rightHand(_)),
    asserta( leftHand(empty) ),
    asserta( rightHand(empty) ),
    retractall( agent_location(_) ),
    asserta( agent_location([1,1])).
    
enter_room(L) :-
    light_location(LL),
    LL = L,!.
enter_room(L) :-
    rightHand(flashlight),!.
enter_room(L) :-
    leftHand(flashlight),!.
enter_room(L) :-
    visited_cells(VC),
    my_member(L,VC),
    !.

    
make_percept([Shout,Flashlight,Weapon,Food,Exit,Giant]):-
    is_hearing_shout(Shout),
    is_seeing_flashlight(Flashlight),
    is_seeing_weapon(Weapon),
    is_seeing_food(Food),
    is_seeing_exit(Exit),
    is_seeing_giant(Giant).
    
is_hearing_shout(yes) :-
    agent_location(X),%agent_location([1,1]).
    giant_location(Y),
    adjacent(X,Y),!.
is_hearing_shout(no).

is_seeing_flashlight(yes) :-
    agent_location(X),
    flashlight_location(Y),
    X = Y,!.
is_seeing_flashlight(no).

is_seeing_weapon(yes):-
    agent_location(X),
    weapon_location(Y),
    X = Y,!.
is_seeing_weapon(no).

is_seeing_food(yes) :-
    agent_location(X),
    food_location(Y),
    X=Y,!.
is_seeing_food(no).

is_seeing_exit(yes) :-
    agent_location(X),
    exit_location(Y),
    X=Y,!.
is_seeing_exit(no).

is_seeing_giant(yes) :-
    agent_location(X),
    giant_location(Y),
    X=Y,!.
is_seeing_giant(no).

    

    
handle_percept([Shout,Flashlight,Weapon,Food,Exit,Giant]):-
    handle_shout(Shout),
    handle_flashlight(Flashlight),
    handle_weapon(Weapon),
    handle_food(Food),
    handle_exit(Exit),
    handle_giant(Giant).

%handle shout
handle_shout(no):-
    agent_location([X,Y]),
    Z1 is Y+1,
    Z2 is Y-1,
    Z3 is X+1,
    Z4 is X-1,
    
    retractall( isGiant(_, [X,Z1]) ),
    asserta( isGiant(no, [X,Z1]) ),
    retractall( isGiant(_, [X,Z2]) ),
    asserta( isGiant(no, [X,Z2]) ),
    retractall( isGiant(_, [Z3,Y]) ),
    asserta( isGiant(no, [Z3,Y]) ) ,
    retractall( isGiant(_, [Z4,Y]) ),
    asserta( isGiant(no, [Z4,Y])).
    
handle_shout(yes):-
    agent_location([X,Y]),
    Z1 is Y+1,
    Z2 is Y-1,
    Z3 is X+1,
    Z4 is X-1,
    (isGiant(no,[X,Z1])->true;retractall(isGiant(_,[X,Z1])),asserta(isGiant(yes,[X,Z1]))),
    (isGiant(no,[X,Z2])->true;retractall(isGiant(_,[X,Z2])),asserta(isGiant(yes,[X,Z2]))),
    (isGiant(no,[Z3,Y])->true;retractall(isGiant(_,[Z3,Y])),asserta(isGiant(yes,[Z3,Y]))),
    (isGiant(no,[Z4,Y])->true;retractall(isGiant(_,[Z4,Y])),asserta(isGiant(yes,[Z4,Y]))).
    
actions([]).

%handle flashlight
handle_flashlight(no):-
    agent_location(AL),
    retractall( isFlashlight(_,AL)),
    asserta( isFlashlight(no,AL)).
    
handle_flashlight(yes):-
    leftHand(empty),
    retractall(leftHand(empty)),
    asserta(leftHand(flashlight)),
    agent_location(AL),
    flashlight_locations(FLL),
    list_delete(AL, FLL, NFLL),
    retractall(flashlight_locations(_)),
    asserta(flashlight_locations(NFLL)),
    retractall(flashlight_location(AL)),
    retractall( isFlashlight(_,AL)),
    asserta( isFlashlight(yes,AL)),
    actions(A),
    list_insert('Hold the flashlight in the left hand',A,NA),
    % list_append('Hold the flashlight in the left hand',A,NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    format('Action: hold the flashlight in the left hand.~n'),
    !.
    
handle_flashlight(yes):-
    rightHand(empty),
    retractall(rightHand(empty)),
    asserta(rightHand(flashlight)),
    agent_location(AL),
    flashlight_locations(FLL),
    list_delete(AL, FLL, NFLL),
    retractall(flashlight_locations(_)),
    asserta(flashlight_locations(NFLL)),
    retractall(flashlight_location(AL)),
    agent_location(AL),
    retractall( isFlashlight(_,AL)),
    asserta( isFlashlight(yes,AL)),
    actions(A),
    list_insert('Hold the flashlight in the right hand',A,NA),
    % list_append('Hold the flashlight in the right hand',A,NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    format('Action: hold the flashlight in the right hand.'),
    !.
    
handle_flashlight(yes):-
    agent_location(AL),
    retractall( isFlashlight(_,AL) ),
    asserta( isFlashlight(yes,AL)),!.
    
%handle weapon
handle_weapon(no):-
    agent_location(AL),
    retractall( isWeapon(_,AL)),
    asserta( isWeapon(no,AL)).
    
handle_weapon(yes):-
    leftHand(empty),
    retractall(leftHand(empty)),
    asserta(leftHand(weapon)),
    agent_location(AL),
    weapon_locations(WL),
    list_delete(AL, WL, NWL),
    retractall(weapon_locations(_)),
    asserta(weapon_locations(NWL)),
    retractall(weapon_location(AL)),
    agent_location(AL),
    retractall( isWeapon(_,AL) ),
    asserta( isWeapon(yes,AL)),
    actions(A),
    list_insert('Hold the weapon in the left hand',A,NA),
    % list_append('Hold the weapon in the left hand',A,NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    format('Action: hold the weapon in the left hand.~n'),
    !.
    
handle_weapon(yes):-
    rightHand(empty),
    retractall(rightHand(empty)),
    asserta(rightHand(weapon)),
    agent_location(AL),
    weapon_locations(WL),
    list_delete(AL, WL, NWL),
    retractall(weapon_locations(_)),
    asserta(weapon_locations(NWL)),
    retractall(weapon_location(AL)),
    agent_location(AL),
    retractall(isWeapon(_,AL)),
    asserta(isWeapon(yes,AL)),
    actions(A),
    list_insert('Hold the weapon in the right hand',A,NA),
    % list_append('Hold the weapon in the right hand',A,NA),
    asserta(actions(NA)),
    format('Action: hold the weapon in the left hand.~n'),
    !.
    
handle_weapon(yes):-
    agent_location(AL),
    retractall( isWeapon(_,AL) ),
    asserta( isWeapon(yes,AL)),!.
    
%handle food
handle_food(no):-
    agent_location(AL),
    retractall( isFood(_,AL)),
    asserta( isFood(no,AL)).
    
handle_food(yes):-
    power(P),
    capacity(C),
    P + 2 < C + 1,
    retractall(power(_)),
    NewP is P+2,
    retractall(power(P)),
    asserta(power(NewP)),
    agent_location(AL),
    food_locations(FL),
    list_delete(AL, FL, NFL),
    retractall(food_locations(_)),
    asserta(food_locations(NFL)),
    retractall( isFood(_,AL) ),
    asserta( isFood(yes,AL)),
    actions(A),
    list_insert('Eat food',A,NA),
    % list_append('Eat food',A,NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    format('Action: Eat food.~n'),
    !.
    
handle_food(yes):-
    agent_location(AL),
    retractall( isFood(_,AL) ),
    asserta( isFood(yes,AL)),!.
    
%handle giant
handle_giant(yes):-
    leftHand(weapon),
    agent_location(AL),
    giant_locations(GL),
    list_delete(AL, GL, NGL),
    retractall(giant_locations(_)),
    asserta(giant_locations(NGL)),
    retractall(giant_location(AL)),
    retractall(isGiant(_,AL)),
    asserta(isGiant(yes,AL)),
    actions(A),
    list_insert('Kill the giant',A,NA),
    % list_append('Kill the giant',A,NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    format('Action: Kill the giant.~n'),
    !.
    
handle_giant(yes):-
    rightHand(weapon),
    agent_location(AL),
    giant_locations(GL),
    list_delete(AL, GL, NGL),
    retractall(giant_locations(_)),
    asserta(giant_locations(NGL)),
    retractall(giant_location(AL)),
    retractall(isGiant(_,AL)),
    asserta(isGiant(yes,AL)),
    actions(A),
    list_insert('Kill the giant',A,NA),
    % list_append('Kill the giant',A,NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    format('Action: Kill the giant.~n'),
    !.

handle_giant(no):-
    agent_location(AL),
    retractall( isGiant(_, AL) ),
    asserta( isGiant(no, AL) ).

%handle exit
handle_exit(no):-
    agent_location(AL),
    retractall( isExit(_, AL) ),
    asserta( isExit(no, AL) ).
    
handle_exit(yes):-
    agent_location(AL),
    retractall( isExit(_, AL) ),
    asserta( isExit(yes, AL) ),
    actions(A),
    list_append('Exit',A,NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    format('Action: Exit.~n')
    .

%Action
do_action(Action) :-
    leftHand(weapon),
    agent_location(AL),
    find_adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    not_member(L,VC),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    actions(A),
    atomics_to_string(L ,',', Loc),
    string_concat('go to ', Loc, S),
    list_append(S, A, NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    Action = L,!.
    
do_action(Action) :-
    rightHand(weapon),
    agent_location(AL),
    find_adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    not_member(L,VC),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    actions(A),
    atomics_to_string(L ,',', Loc),
    string_concat('go to ', Loc, S),
    list_append(S, A, NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    Action = L,!.
    
do_action(Action) :-
    isGiant(no,L),
    agent_location(AL),
    adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    not_member(L,VC),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    actions(A),
    atomics_to_string(L ,',', Loc),
    string_concat('go to ', Loc, S),
    list_append(S, A, NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    Action = L,!.
    
do_action(Action) :-
    agent_location(AL),
    find_adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    my_member(L,VC),
    leftHand(weapon),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    actions(A),
    atomics_to_string(L ,',', Loc),
    string_concat('go to ', Loc, S),
    list_append(S, A, NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    Action = L,!.
    
do_action(Action) :-
    agent_location(AL),
    find_adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    my_member(L,VC),
    rightHand(weapon),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    actions(A),
    atomics_to_string(L ,',', Loc),
    string_concat('go to ', Loc, S),
    list_append(S, A, NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    Action = L,!.
    
do_action(Action) :-
    isGiant(no,L),
    agent_location(AL),
    adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    my_member(L,VC),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    actions(A),
    atomics_to_string(L ,',', Loc),
    string_concat('go to ', Loc, S),
    list_append(S, A, NA),
    retractall(actions(A)),
    asserta(actions(NA)),
    Action = L,!.
    

% cosume power rate   
consum_power(X):-
    leftHand(empty),
    rightHand(empty),
    X is 1,!.
  
consum_power(X):-
    leftHand(empty),
    not(rightHand(empty)),
    X is 2,!.
    
consum_power(X):-
    rightHand(empty),
    not(leftHand(empty)),
    X is 2,!.
    
consum_power(X):-
    not(leftHand(empty)),
    not(rightHand(empty)),
    X is 4,!.

    
adjacent([X2,Y2],[X1,Y1]):-
N is abs(X1-X2),M is abs(Y1-Y2),(N = 1,M = 0),!.
adjacent([X2,Y2],[X1,Y1]):-
N is abs(X1-X2),M is abs(Y1-Y2),(N = 0,M = 1),!.

find_adjacent([X,Y],[Z,W]):-
    Z is X+1,
    W is Y.
    
find_adjacent([X,Y],[Z,W]):-
    Z is X-1,
    W is Y.
    
find_adjacent([X,Y],[Z,W]):-
    Z is X,
    W is Y+1.

find_adjacent([X,Y],[Z,W]):-
    Z is X,
    W is Y-1.
    
% check permitted cells to go
permitted([X,Y]) :-
    world_rows(Row),
    world_rows(Col),
    0 < X, X < Row+1,
    0 < Y, Y < Col+1.
    

not_member(X, []).
not_member([X,Y], [[U,V]|Ys]) :-
    ( X=U,Y=V -> fail
    ; not_member([X,Y], Ys)
).

%  membership
my_member(X, []):- fail.
my_member([X,Y], [[U,V]|Ys]) :-
    ( X=U,Y=V -> true
    ; member([X,Y], Ys)
).

% delete from list
list_delete(X, [X], []).
list_delete(X,[X|L1], L1).
list_delete(X, [Y|L2], [Y|L1]) :- list_delete(X,L2,L1).


% append to list
list_member(X,[X|_]).
list_member(X,[_|TAIL]) :- list_member(X,TAIL).
list_append(A,T,T) :- list_member(A,T),!.
list_append(A,T,[A|T]).

% Reverse List
list_concat([],L,L).
list_concat([X1|L1],L2,[X1|L3]) :- list_concat(L1,L2,L3).
list_rev([],[]).
list_rev([Head|Tail],Reversed) :-
   list_rev(Tail, RevTail),list_concat(RevTail, [Head],Reversed).

%  insert list
list_delete(X, [X], []).
list_delete(X,[X|L1], L1).
list_delete(X, [Y|L2], [Y|L1]) :- list_delete(X,L2,L1).

list_insert(X,L,R) :- list_delete(X,R,L).