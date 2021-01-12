:- module(iaib200582, [iaib200582/3]).

debug_mode(false).

iaib200582(Color,X,Y):-
    color(Color,Player),
    board_snapshot(Board),
    choose_move(Board,Player,X,Y,0,Move),!,
    play_move(Move).

iaib200582(_,_,_).

opponent(white, black).
opponent(black, white).

dir(white,1):- !.
dir(black,-1).

color(1,white).
color(2,black).

choose_move(Board,Player,0,0,Depth,Move):-
    get_all_moves(Board,Player,Moves),
    % best(Player,Player,Moves,Move,_,Depth),!.
    bounded_best(Player,Player,-1000,1000,Moves,Move,_,Depth),!.

choose_move(Board,Player,X,Y,Depth,Move):-
    findall(Move,(dir(Player,Dir),chain_capture(Board,X,Y,Dir,Move),Move \= []),Moves),
    % best(Player,Player,Moves,Move,_,Depth),!.
    bounded_best(Player,Player,-1000,1000,Moves,Move,_,Depth),!.

play_move(m(X,Y,X1,Y1,_)):- make_move(X,Y,X1,Y1).
play_move([c(X,Y,X1,Y1,X2,Y2,_)|_]):- make_capture(X,Y,X1,Y1,X2,Y2).

make_capture(X,Y,X1,Y1,X2,Y2):-
    retract(ruut(X1, Y1, _)),
    assert(ruut(X1,Y1,0)),
    make_move(X,Y,X2,Y2).

make_move(X,Y,X1,Y1):- 
    retract(ruut(X, Y, Color)),
    retract(ruut(X1, Y1, _)), 
    assert(ruut(X, Y, 0)), 
    make_promotion(Color,X1,C),
    assert(ruut(X1, Y1, C)).

make_promotion(1,8,10):- !.
make_promotion(2,1,20):- !.
make_promotion(Color,_,Color).

board_snapshot(Board):-
    findall(Piece,(ruut(X,Y,Color),create_piece(X,Y,Color,Piece)), Board). 

create_piece(X,Y,1,piece(white,man,X,Y)).
create_piece(X,Y,2,piece(black,man,X,Y)).
create_piece(X,Y,10,piece(white,queen,X,Y)).
create_piece(X,Y,20,piece(black,queen,X,Y)).

get_all_moves(Board,Color,Moves):-
    get_all_pieces(Board,Color,Pieces),
    get_all_moves_helper(Board,Pieces,Moves),
    Moves \= [].

get_all_moves_helper(Board,Pieces,Moves):-
    get_possible_capture_moves(Board,Pieces,Moves), Moves \= [],!.

get_all_moves_helper(Board,Pieces,Moves):-
    get_possible_moves(Board,Pieces,Moves).

get_all_pieces(Board,Color,Pieces):-
    findall(piece(Color,Type,X,Y),member(piece(Color,Type,X,Y),Board),Pieces).

get_possible_capture_moves(_,[],[]):-!.

get_possible_capture_moves(Board,[piece(Color,_,X,Y)|Pieces],Moves):-
    findall(Move,(dir(Color,Dir),chain_capture(Board,X,Y,Dir,Move),Move \= []),Captures),!,
    get_possible_capture_moves(Board,Pieces,R),
    append(Captures,R,Moves).

get_possible_capture_moves(Board,[_|Pieces],Moves):-
    get_possible_capture_moves(Board,Pieces,Moves).

get_possible_moves(_,[],[]):-!.

get_possible_moves(Board,[piece(Color,_,X,Y)|Pieces],Moves):-
    findall(Move,(dir(Color,Dir),can_move(Board,X,Y,Dir,_,_,Move)),M),!,
    get_possible_moves(Board,Pieces,R),
    append(M,R,Moves).

get_possible_moves(Board,[_|Pieces],Moves):-
    get_possible_moves(Board,Pieces,Moves).

chain_capture(Board,X,Y,Dir,[c(X,Y,X1,Y1,X2,Y2,NewBoard)|Moves]):-
    can_capture(Board,X,Y,Dir,X1,Y1,X2,Y2,c(X,Y,X1,Y1,X2,Y2,NewBoard)),
    chain_capture(NewBoard,X2,Y2,Dir,Moves).

chain_capture(Board,X,Y,Dir,[]):-
    \+ can_capture(Board,X,Y,Dir,_,_,_,_,_).

can_capture(Board,X,Y,Dir,X1,Y1,X2,Y2,c(X,Y,X1,Y1,X2,Y2,NewBoard)):- % forward right
    X1 is X + Dir,
    Y1 is Y + 1,
    member(piece(Color,man,X,Y),Board),
    opponent(Color,Opponent),
    member(piece(Opponent,_,X1,Y1),Board),
    X2 is X1 + Dir,
    Y2 is Y1 + 1,
    is_empty(X2,Y2,Board),
    capture(Board,X,Y,X1,Y1,X2,Y2,NewBoard).

can_capture(Board,X,Y,Dir,X1,Y1,X2,Y2,c(X,Y,X1,Y1,X2,Y2,NewBoard)):- % forward left
    X1 is X + Dir,
    Y1 is Y - 1,
    member(piece(Color,man,X,Y),Board),
    opponent(Color,Opponent),
    member(piece(Opponent,_,X1,Y1),Board),
    X2 is X1 + Dir,
    Y2 is Y1 - 1,
    is_empty(X2,Y2,Board),
    capture(Board,X,Y,X1,Y1,X2,Y2,NewBoard).

can_capture(Board,X,Y,Dir,X1,Y1,X2,Y2,c(X,Y,X1,Y1,X2,Y2,NewBoard)):- % backwards right
    X1 is X + Dir * -1,
    Y1 is Y + 1,
    member(piece(Color,man,X,Y),Board),
    opponent(Color,Opponent),
    member(piece(Opponent,_,X1,Y1),Board),
    X2 is X1 + Dir * -1,
    Y2 is Y1 + 1,
    is_empty(X2,Y2,Board),
    capture(Board,X,Y,X1,Y1,X2,Y2,NewBoard).

can_capture(Board,X,Y,Dir,X1,Y1,X2,Y2,c(X,Y,X1,Y1,X2,Y2,NewBoard)):- % backwards left
    X1 is X + Dir * -1,
    Y1 is Y - 1,
    member(piece(Color,man,X,Y),Board),
    opponent(Color,Opponent),
    member(piece(Opponent,_,X1,Y1),Board),
    X2 is X1 + Dir * -1,
    Y2 is Y1 - 1,
    is_empty(X2,Y2,Board),
    capture(Board,X,Y,X1,Y1,X2,Y2,NewBoard).

can_capture(Board,X,Y,_,X1,Y1,X2,Y2,c(X,Y,X1,Y1,X2,Y2,NewBoard)):-
    member(piece(Color,queen,X,Y),Board),
    opponent(Color,Opponent),
    member(piece(Opponent,_,X1,Y1),Board),
    DiffX is abs(X - X1), DiffY is abs(Y - Y1),
    DiffX == DiffY,
    Dx is sign(X1 - X), Dy is sign(Y1 - Y),
    GoalX is X1 - Dx, GoalY is Y1 - Dy,
    check_diagonal(Board,X,Y,GoalX,GoalY,Dx,Dy),
    get_post_positions(Board,X1,Y1,Dx,Dy,Positions),Positions \= [],
    member(p(X2,Y2),Positions),     
    capture(Board,X,Y,X1,Y1,X2,Y2,NewBoard).

get_post_positions(Board,StartX,StartY,Dx,Dy,[p(X2,Y2)|Positions]):-
    X2 is StartX + Dx,
    Y2 is StartY + Dy,
    is_empty(X2,Y2,Board),
    get_post_positions(Board,X2,Y2,Dx,Dy,Positions).

get_post_positions(Board,StartX,StartY,Dx,Dy,[]):-
    X2 is StartX + Dx,
    Y2 is StartY + Dy,
    \+ is_empty(X2,Y2,Board),!.

check_diagonal(_,X,Y,X,Y,_,_):- !.

check_diagonal(Board,X,Y,X1,Y1,Dx,Dy):-
    X2 is X + Dx,
    Y2 is Y + Dy,
    is_empty(X2,Y2,Board),
    check_diagonal(Board,X2,Y2,X1,Y1,Dx,Dy).

can_move(Board,X,Y,Dir,X1,Y1,m(X,Y,X1,Y1,NewBoard)):-
    member(piece(_,man,X,Y),Board),
    X1 is X + Dir,
    Y1 is Y + 1,
    is_empty(X1,Y1,Board),
    move(Board,X,Y,X1,Y1,NewBoard).

can_move(Board,X,Y,Dir,X1,Y1,m(X,Y,X1,Y1,NewBoard)):-
    member(piece(_,man,X,Y),Board),
    X1 is X + Dir,
    Y1 is Y - 1,
    is_empty(X1,Y1,Board),
    move(Board,X,Y,X1,Y1,NewBoard).

can_move(Board,X,Y,_,X1,Y1,m(X,Y,X1,Y1,NewBoard)):- 
    member(piece(_,queen,X,Y),Board),
    get_post_positions(Board,X,Y,1,1,Positions),Positions \= [],
    member(p(X1,Y1),Positions),
    move(Board,X,Y,X1,Y1,NewBoard).

can_move(Board,X,Y,_,X1,Y1,m(X,Y,X1,Y1,NewBoard)):- 
    member(piece(_,queen,X,Y),Board),
    get_post_positions(Board,X,Y,-1,-1,Positions),Positions \= [],
    member(p(X1,Y1),Positions),
    move(Board,X,Y,X1,Y1,NewBoard).

can_move(Board,X,Y,_,X1,Y1,m(X,Y,X1,Y1,NewBoard)):- 
    member(piece(_,queen,X,Y),Board),
    get_post_positions(Board,X,Y,-1,1,Positions),Positions \= [],
    member(p(X1,Y1),Positions),
    move(Board,X,Y,X1,Y1,NewBoard).

can_move(Board,X,Y,_,X1,Y1,m(X,Y,X1,Y1,NewBoard)):- 
    member(piece(_,queen,X,Y),Board),
    get_post_positions(Board,X,Y,1,-1,Positions),Positions \= [],
    member(p(X1,Y1),Positions),
    move(Board,X,Y,X1,Y1,NewBoard).

move(Board,X,Y,X1,Y1,NewBoard):-
    member(piece(Color,Type,X,Y),Board),!,
    delete(Board,piece(Color,Type,X,Y),Temp),
    promotion(Color,Type,X1,T),
    append(Temp,[piece(Color,T,X1,Y1)],NewBoard).

capture(Board,X,Y,X1,Y1,X2,Y2,NewBoard):-
    delete(Board,piece(_,_,X1,Y1),Temp),
    move(Temp,X,Y,X2,Y2,NewBoard).

is_empty(X,Y,Board):-
    between(1,8,X),
    between(1,8,Y),
    \+ member(piece(_,_,X,Y),Board).

promotion(white,man,8,queen):- !.
promotion(black,man,1,queen):- !.
promotion(_,Type,_,Type).

count_men(Board,Color,Count):- 
    count_pieces(Board,Color,man,Count).

count_queens(Board,Color,Count):-
    count_pieces(Board,Color,queen,Count).

count_pieces(Board,Color,Type,Count):-
    findall(piece(Color,Type,_,_),member(piece(Color,Type,_,_),Board),Bag),
    length(Bag, Count).

static_evaluation(Board,Player,Score):-
    opponent(Player, Opponent),
    count_men(Board,Player,PlayerMen),
    count_queens(Board,Player,PlayerKings),
    count_men(Board,Opponent,OpponentMen),
    count_queens(Player,Opponent,OpponentKings),
    Score is PlayerMen + PlayerKings * 2 - OpponentMen + OpponentKings * 2.

get_board(m(_,_,_,_,Board),Board).
get_board(c(_,_,_,_,_,_,Board),Board).

extract_move([Move],Move).
extract_move([_|Moves],Last):- reverse(Moves, [Last|_]).
extract_move(Move,Move).

alpha_beta(Board,Player,CurrentPlayer,Alpha,Beta,NextMove,Score,Depth):-
    Depth < 5,
    D is Depth + 1,
    get_all_moves(Board,CurrentPlayer,Moves),
    bounded_best(Player,CurrentPlayer,Alpha,Beta,Moves,NextMove,Score,D),!.

alpha_beta(Board,Player,_,_,_,_,Score,_):-
    static_evaluation(Board,Player,Score),!.

bounded_best(Player,CurrentPlayer,Alpha,Beta,[Move|Moves],BestMove,BestScore,Depth):-
    extract_move(Move,M),
    get_board(M,Board),
    ((debug_mode(true),
    nl,
    write('Depth: '),
    write(Depth),
    write('.Player: '),
    write(CurrentPlayer),
    nl,
    print_board(Board));
    true),
    opponent(CurrentPlayer,Opponent),
    alpha_beta(Board,Player,Opponent,Alpha,Beta,_,Score,Depth),
    ((debug_mode(true),
    nl,
    write('Score: '),
    write(Score),
    nl);
    true),
    good_enough(Player,CurrentPlayer,Moves,Alpha,Beta,Move,Score,BestMove,BestScore,Depth).

good_enough(_,_,[],_,_,Move,Score,Move,Score,_):-!.

good_enough(Player,CurrentPlayer,_,_,Beta,Move,Score,Move,Score,_):- 
    \+ maximizing(Player,CurrentPlayer),
    Score > Beta,!.

good_enough(Player,CurrentPlayer,_,Alpha,_,Move,Score,Move,Score,_):-
    maximizing(Player,CurrentPlayer),
    Score < Alpha,!.

good_enough(Player,CurrentPlayer,Moves,Alpha,Beta,Move,Score,BestMove,BestScore,Depth):-
    new_bounds(Player,CurrentPlayer,Alpha,Beta,Score,NewAlpha,NewBeta),
    bounded_best(Player,CurrentPlayer,NewAlpha,NewBeta,Moves,Move1,Score1,Depth),
    better_of(Player,CurrentPlayer,Move,Score,Move1,Score1,BestMove,BestScore).

new_bounds(Player,CurrentPlayer,Alpha,Beta,Score,Score,Beta):-
    \+ maximizing(Player,CurrentPlayer),
    Score > Alpha,!.

new_bounds(Player,CurrentPlayer,Alpha,Beta,Score,Alpha,Score):-
    maximizing(Player,CurrentPlayer),
    Score < Beta,!.

new_bounds(_,_,Alpha,Beta,_,Alpha,Beta).

minimax(Board,Player,CurrentPlayer,NextMove,Score,Depth):-
    Depth < 5,
    D is Depth + 1,
    get_all_moves(Board,CurrentPlayer,Moves),
    best(Player,CurrentPlayer,Moves,NextMove,Score,D),!.

minimax(Board,Player,_,_,Score,_):-
    static_evaluation(Board,Player,Score),!.

best(Player,CurrentPlayer,[Move],Move,Score,Depth):-
    extract_move(Move,M),
    get_board(M,Board),
    ((debug_mode(true),
    nl,
    write('Depth: '),
    write(Depth),
    write('.Player: '),
    write(CurrentPlayer),
    nl,
    print_board(Board));
    true),
    opponent(CurrentPlayer,Opponent),
    minimax(Board,Player,Opponent,_,Score,Depth),!.

best(Player,CurrentPlayer,[Move|Moves],BestMove,BestScore,Depth):-
    extract_move(Move,M),
    get_board(M,Board),
    ((debug_mode(true),
    nl,
    write('Depth: '),
    write(Depth),
    write('.Player: '),
    write(CurrentPlayer),
    nl,
    print_board(Board));
    true),
    opponent(CurrentPlayer,Opponent),
    minimax(Board,Player,Opponent,_,Score,Depth),
    ((debug_mode(true),
    nl,
    write('Score: '),
    write(Score),
    nl);
    true),
    best(Player,CurrentPlayer,Moves,BestMove1,BestScore1,Depth),
    better_of(Player,CurrentPlayer,Move,Score,BestMove1,BestScore1,BestMove,BestScore).

better_of(Player,CurrentPlayer,Move1,Score1,_,Score2,Move1,Score1):-
    maximizing(Player,CurrentPlayer),
    Score1 >= Score2,!.

better_of(Player,CurrentPlayer,_,Score1,Move2,Score2,Move2,Score2):-
    maximizing(Player,CurrentPlayer),
    Score1 =< Score2,!.

better_of(Player,CurrentPlayer,_,Score1,Move2,Score2,Move2,Score2):-
    \+ maximizing(Player,CurrentPlayer),
    Score1 >= Score2,!.

better_of(Player,CurrentPlayer,Move1,Score1,_,Score2,Move1,Score1):-
    \+ maximizing(Player,CurrentPlayer),
    Score1 =< Score2,!.

maximizing(Player,Player).

piece_value(white,man,'\u2659').
piece_value(black,man,'\u265F').
piece_value(white,queen,'\u2655').
piece_value(black,queen,'\u265B').

print_boards([]):-!.
print_boards([Board|Tail]):-
    print_board(Board),
    print_boards(Tail).

print_board(Board):-
    nl,
    print_row(Board,8),
    print_row(Board,7),
    print_row(Board,6),
    print_row(Board,5),
    print_row(Board,4),
    print_row(Board,3),
    print_row(Board,2),
    print_row(Board,1).

print_row(Board,X):-
    write('row # '),write(X), write('   '),
    print_square(Board,X,1),
    print_square(Board,X,2),
    print_square(Board,X,3),
    print_square(Board,X,4),
    print_square(Board,X,5),
    print_square(Board,X,6),
    print_square(Board,X,7),
    print_square(Board,X,8),
    nl.

print_square(Board,X,Y):-
    (
        member(piece(Color,Type,X,Y),Board),!,
        piece_value(Color,Type,Val),!,
        write(Val));
    write('#').
