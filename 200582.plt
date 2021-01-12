:- begin_tests(iaib200582).
:- include('200582').

test(board_snapshot, [cleanup(abolish(ruut/3))]):-
	init_squares,
	initial_board(Board),
    board_snapshot(Board).

test(board_snapshot_2, [cleanup(abolish(ruut/3))]):-
	test_case_1,
	board_snapshot(Board),
	print_board(Board),
	Board = [
		piece(white,man,3,1),
		piece(white,man,2,2),
		piece(white,man,2,4),
		piece(white,man,2,8),
		piece(white,man,1,5),
		piece(white,man,3,5),
		piece(white,man,3,7),
		piece(white,man,4,6),
		piece(black,man,6,2),
		piece(black,man,7,3),
		piece(black,man,7,5)
	].

test(move):-
	initial_board(Board),
	move(Board,3,1,4,2,[ 
		piece(white,man,1,1),
		piece(white,man,1,3),
		piece(white,man,1,5),
		piece(white,man,1,7),
		piece(white,man,2,2),
		piece(white,man,2,4),
		piece(white,man,2,6),
		piece(white,man,2,8),
		piece(white,man,3,3),
		piece(white,man,3,5),
		piece(white,man,3,7),
		piece(black,man,6,2),
		piece(black,man,6,4),
		piece(black,man,6,6),
		piece(black,man,6,8),
		piece(black,man,7,1),
		piece(black,man,7,3),
		piece(black,man,7,5),
		piece(black,man,7,7),
		piece(black,man,8,2),
		piece(black,man,8,4),
		piece(black,man,8,6),
		piece(black,man,8,8),
		piece(white,man,4,2)
  ]).

test(move_promotion_white):-
	move([piece(white,man,7,7)],7,7,8,8,[piece(white,queen,8,8)]).

test(move_promotion_black):-
	move([piece(black,man,2,2)],2,2,1,1,[piece(black,queen,1,1)]).

test(capture):-
	initial_board(Board),
	move(Board,3,1,4,2,Next),
	move(Next,6,4,5,3,NewBoard),
	capture(NewBoard,4,2,5,3,6,4,[ 
		piece(white,man,1,1),
		piece(white,man,1,3),
		piece(white,man,1,5),
		piece(white,man,1,7),
		piece(white,man,2,2),
		piece(white,man,2,4),
		piece(white,man,2,6),
		piece(white,man,2,8),
		piece(white,man,3,3),
		piece(white,man,3,5),
		piece(white,man,3,7),
		piece(black,man,6,2),
		piece(black,man,6,6),
		piece(black,man,6,8),
		piece(black,man,7,1),
		piece(black,man,7,3),
		piece(black,man,7,5),
		piece(black,man,7,7),
		piece(black,man,8,2),
		piece(black,man,8,4),
		piece(black,man,8,6),
		piece(black,man,8,8),
		piece(white,man,6,4)
	]).

test(can_move_white):-
	initial_board(Board),
	dir(white,Dir),
	findall(Move,can_move(Board,3,5,Dir,_,_,Move),[
		m(3,5,4,6,
		[ piece(white,man,1,1),
		  piece(white,man,1,3),
		  piece(white,man,1,5),
		  piece(white,man,1,7),
		  piece(white,man,2,2),
		  piece(white,man,2,4),
		  piece(white,man,2,6),
		  piece(white,man,2,8),
		  piece(white,man,3,1),
		  piece(white,man,3,3),
		  piece(white,man,3,7),
		  piece(black,man,6,2),
		  piece(black,man,6,4),
		  piece(black,man,6,6),
		  piece(black,man,6,8),
		  piece(black,man,7,1),
		  piece(black,man,7,3),
		  piece(black,man,7,5),
		  piece(black,man,7,7),
		  piece(black,man,8,2),
		  piece(black,man,8,4),
		  piece(black,man,8,6),
		  piece(black,man,8,8),
		  piece(white,man,4,6)
		]),
	  m(3,5,4,4,
		[ piece(white,man,1,1),
		  piece(white,man,1,3),
		  piece(white,man,1,5),
		  piece(white,man,1,7),
		  piece(white,man,2,2),
		  piece(white,man,2,4),
		  piece(white,man,2,6),
		  piece(white,man,2,8),
		  piece(white,man,3,1),
		  piece(white,man,3,3),
		  piece(white,man,3,7),
		  piece(black,man,6,2),
		  piece(black,man,6,4),
		  piece(black,man,6,6),
		  piece(black,man,6,8),
		  piece(black,man,7,1),
		  piece(black,man,7,3),
		  piece(black,man,7,5),
		  piece(black,man,7,7),
		  piece(black,man,8,2),
		  piece(black,man,8,4),
		  piece(black,man,8,6),
		  piece(black,man,8,8),
		  piece(white,man,4,4)
		])
	]).

test(can_move_black):-
	initial_board(Board),
	dir(black,Dir),
	findall(Move,can_move(Board,6,2,Dir,_,_,Move),
	[ m(6,2,5,3,
		[ piece(white,man,1,1),
		  piece(white,man,1,3),
		  piece(white,man,1,5),
		  piece(white,man,1,7),
		  piece(white,man,2,2),
		  piece(white,man,2,4),
		  piece(white,man,2,6),
		  piece(white,man,2,8),
		  piece(white,man,3,1),
		  piece(white,man,3,3),
		  piece(white,man,3,5),
		  piece(white,man,3,7),
		  piece(black,man,6,4),
		  piece(black,man,6,6),
		  piece(black,man,6,8),
		  piece(black,man,7,1),
		  piece(black,man,7,3),
		  piece(black,man,7,5),
		  piece(black,man,7,7),
		  piece(black,man,8,2),
		  piece(black,man,8,4),
		  piece(black,man,8,6),
		  piece(black,man,8,8),
		  piece(black,man,5,3)
		]),
	  m(6,2,5,1,
		[ piece(white,man,1,1),
		  piece(white,man,1,3),
		  piece(white,man,1,5),
		  piece(white,man,1,7),
		  piece(white,man,2,2),
		  piece(white,man,2,4),
		  piece(white,man,2,6),
		  piece(white,man,2,8),
		  piece(white,man,3,1),
		  piece(white,man,3,3),
		  piece(white,man,3,5),
		  piece(white,man,3,7),
		  piece(black,man,6,4),
		  piece(black,man,6,6),
		  piece(black,man,6,8),
		  piece(black,man,7,1),
		  piece(black,man,7,3),
		  piece(black,man,7,5),
		  piece(black,man,7,7),
		  piece(black,man,8,2),
		  piece(black,man,8,4),
		  piece(black,man,8,6),
		  piece(black,man,8,8),
		  piece(black,man,5,1)
		])
	]).

test(can_move_white_edge):-
	initial_board(Board),
	dir(white,Dir),
	findall(Move,can_move(Board,3,1,Dir,_,_,Move),Bag),
	length(Bag,1).

test(can_move_black_edge):-
	initial_board(Board),
	dir(black,Dir),
	findall(Move,can_move(Board,6,8,Dir,_,_,Move),Bag),
	length(Bag,1).

test(can_capture_simple_white):-
	initial_board(Board),
	move(Board,3,1,4,2,Next),
	move(Next,6,4,5,3,NewBoard),
	dir(white,Dir),
	findall(Move,can_capture(NewBoard,4,2,Dir,_,_,_,_,Move),[ 
		c(4,2,5,3,6,4,
		[ piece(white,man,1,1),
		  piece(white,man,1,3),
		  piece(white,man,1,5),
		  piece(white,man,1,7),
		  piece(white,man,2,2),
		  piece(white,man,2,4),
		  piece(white,man,2,6),
		  piece(white,man,2,8),
		  piece(white,man,3,3),
		  piece(white,man,3,5),
		  piece(white,man,3,7),
		  piece(black,man,6,2),
		  piece(black,man,6,6),
		  piece(black,man,6,8),
		  piece(black,man,7,1),
		  piece(black,man,7,3),
		  piece(black,man,7,5),
		  piece(black,man,7,7),
		  piece(black,man,8,2),
		  piece(black,man,8,4),
		  piece(black,man,8,6),
		  piece(black,man,8,8),
		  piece(white,man,6,4)
		])
	]).

test(can_capture_simple_black):-
	initial_board(Board),
	print_board(Board),
	move(Board,3,1,4,2,Next),
	move(Next,6,6,5,5,Next1),
	move(Next1,3,7,4,6,NewBoard),
	print_board(NewBoard),
	dir(black,Dir),
	findall(Move,can_capture(NewBoard,5,5,Dir,_,_,_,_,Move),[c(5,5,4,6,3,7,B)|[]]),
	print_board(B).

test(can_capture_all_dirs_white):-
	Board = [
		piece(white,man,4,4),
		piece(black,man,3,3),
		piece(black,man,5,5),
		piece(black,man,5,3),
		piece(black,man,3,5)
	],
	print_board(Board),
	dir(white,Dir),
	findall(B,can_capture(Board,4,4,Dir,_,_,_,_,c(_,_,_,_,_,_,B)),Boards),
	length(Boards,4),
	print_boards(Boards).

test(can_capture_all_dirs_black):-
	Board = [
		piece(black,man,5,5),
		piece(white,man,6,6),
		piece(white,man,4,4),
		piece(white,man,6,4),
		piece(white,man,4,6)
	],
	print_board(Board),
	dir(black,Dir),
	findall(B,can_capture(Board,5,5,Dir,_,_,_,_,c(_,_,_,_,_,_,B)),Boards),
	length(Boards,4),
	print_boards(Boards).

test(can_capture_all_dirs_obs_white):-
	Board = [
		piece(white,man,4,4),
		piece(black,man,3,3),
		piece(black,man,5,5),
		piece(black,man,6,6),
		piece(black,man,5,3),
		piece(black,man,3,5),
		piece(white,man,2,6)
	],
	print_board(Board),
	dir(white,Dir),
	findall(B,can_capture(Board,4,4,Dir,_,_,_,_,c(_,_,_,_,_,_,B)),Boards),
	length(Boards,2),
	print_boards(Boards).

test(can_capture_all_dirs_obs_black):-
	Board = [
		piece(black,man,5,5),
		piece(white,man,6,6),
		piece(white,man,4,4),
		piece(white,man,3,3),
		piece(white,man,6,4),
		piece(black,man,7,3),
		piece(white,man,4,6)
	],
	print_board(Board),
	dir(black,Dir),
	findall(B,can_capture(Board,5,5,Dir,_,_,_,_,c(_,_,_,_,_,_,B)),Boards),
	length(Boards,2),
	print_boards(Boards).

test(can_capture_edge_white):-
	Board = [
		piece(white,man,2,4),
		piece(black,man,1,5),
		piece(black,man,3,3),
		piece(black,man,3,5)
	],
	print_board(Board),
	dir(white,Dir),
	findall(B,can_capture(Board,2,4,Dir,_,_,_,_,c(_,_,_,_,_,_,B)),Boards),
	length(Boards,2),
	print_boards(Boards).

test(can_capture_edge_black):-
	Board = [
		piece(black,man,6,2),
		piece(white,man,7,1),
		piece(white,man,5,3)
	],
	print_board(Board),
	dir(black,Dir),
	findall(B,can_capture(Board,6,2,Dir,_,_,_,_,c(_,_,_,_,_,_,B)),Boards),
	length(Boards,1),
	print_boards(Boards).

test(chain_capture_diag_two_pcs_white):-
	Board = [
		piece(white,man,4,4),
		piece(black,man,5,5),
		piece(black,man,7,7)
	],
	print_board(Board),
	dir(white,Dir),
	findall(Capture,chain_capture(Board,4,4,Dir,Capture),[Move|[]]),
	length(Move,2),
	reverse(Move, [c(_,_,_,_,_,_,B)|_]),
	print_board(B).

test(chain_capture_mult_opt_black):-
	Board = [
		piece(black,man,7,5),
		piece(white,man,6,4),
		piece(white,man,4,2),
		piece(white,man,4,4),
		piece(white,man,2,6)
	],
	print_board(Board),
	dir(black,Dir),
	findall(Capture,chain_capture(Board,7,5,Dir,Capture),Moves),
	length(Moves,2).

test(chain_capture_mult_and_single_white):-
	Board = [
		piece(white,man,3,7),
		piece(black,man,2,6),
		piece(black,man,4,6),
		piece(black,man,6,6)
	],
	print_board(Board),
	dir(white,Dir),
	findall(Capture,chain_capture(Board,3,7,Dir,Capture),Moves),
	length(Moves,2).

test(chain_capture_single_black):-
	Board = [
		piece(black,man,8,2),
		piece(white,queen,7,3),
		piece(white,man,3,1)
	],
	print_board(Board),
	dir(black,Dir),
	findall(Capture,chain_capture(Board,8,2,Dir,Capture),Moves),
	length(Moves,1).

test(get_all_pieces_white):-
	initial_board(Board),
	print_board(Board),
	get_all_pieces(Board,white,Pieces),
	length(Pieces,12).

test(get_all_pieces_black):-
	initial_board(Board),
	print_board(Board),
	get_all_pieces(Board,black,Pieces),
	length(Pieces,12).

test(get_all_pieces_b_1):-
	Board = [
		piece(black,man,7,5),
		piece(white,man,6,4),
		piece(white,man,4,2),
		piece(white,man,4,4),
		piece(white,man,2,6)
	],
	print_board(Board),
	get_all_pieces(Board,black,Pieces),
	length(Pieces,1).

test(get_all_pieces_w_1):-
	Board = [
		piece(black,man,5,5),
		piece(white,man,6,6),
		piece(white,man,4,4),
		piece(white,man,3,3),
		piece(white,man,6,4),
		piece(black,man,7,3),
		piece(white,man,4,6)
	],
	print_board(Board),
	get_all_pieces(Board,white,Pieces),
	length(Pieces,5).

test(get_possible_capture_moves_white):-
	Board = [
		piece(white,man,3,5),
		piece(black,man,4,4),
		piece(black,man,6,2),
		piece(black,man,4,6),
		piece(white,man,3,3),
		piece(black,man,4,2)
	],
	print_board(Board),
	get_all_pieces(Board,white,Pieces),
	get_possible_capture_moves(Board,Pieces,Moves),
	length(Moves,5).

test(get_possible_capture_moves_blocked_white):-
	Board = [
		piece(white,man,1,1),
		piece(white,man,2,2),
		piece(white,man,3,3),
		piece(black,man,4,2),
		piece(black,man,4,4),
		piece(black,man,6,6)
	],
	print_board(Board),
	get_all_pieces(Board,white,Pieces),
	get_possible_capture_moves(Board,Pieces,Moves),
	length(Moves,2).

test(get_possible_capture_moves_blocked_black):-
	Board = [
		piece(black,man,7,3),
		piece(black,man,6,2),
		piece(black,man,6,4),
		piece(white,man,5,1),
		piece(white,man,5,3),
		piece(white,man,5,5)
	],
	print_board(Board),
	get_all_pieces(Board,black,Pieces),
	get_possible_capture_moves(Board,Pieces,Moves),
	length(Moves,3).

test(get_possible_moves_init_white):-
	initial_board(Board),
	print_board(Board),
	get_all_pieces(Board,white,Pieces),
	get_possible_moves(Board,Pieces,Moves),
	length(Moves,7).

test(get_possible_moves_init_black):-
	initial_board(Board),
	print_board(Board),
	get_all_pieces(Board,black,Pieces),
	get_possible_moves(Board,Pieces,Moves),
	length(Moves,7).

test(get_possible_moves_blocked_white):-
	Board = [
		piece(white,man,3,3),
		piece(black,man,4,4),
		piece(black,man,5,5)
	],
	print_board(Board),
	get_all_pieces(Board,white,Pieces),
	get_possible_moves(Board,Pieces,Moves),
	length(Moves,1).

test(get_possible_moves_blocked_black):-
	Board = [
		piece(black,man,6,2),
		piece(black,man,5,1),
		piece(white,man,4,2),
		piece(white,man,3,3)
	],
	print_board(Board),
	get_all_pieces(Board,black,Pieces),
	get_possible_moves(Board,Pieces,Moves),
	length(Moves,1).

test(get_possible_moves_blocked_black):-
	Board = [
		piece(black,man,6,2),
		piece(black,man,5,1),
		piece(white,man,4,2),
		piece(white,man,3,3)
	],
	print_board(Board),
	get_all_pieces(Board,black,Pieces),
	get_possible_moves(Board,Pieces,Moves),
	length(Moves,1).

test(get_possible_moves_edges_black):-
	Board = [
		piece(black,man,8,8),
		piece(black,man,1,1)
	],
	print_board(Board),
	get_all_pieces(Board,black,Pieces),
	get_possible_moves(Board,Pieces,Moves),
	length(Moves,1).

test(get_possible_moves_edges_white):-
	Board = [
		piece(white,man,8,8),
		piece(white,man,1,1)
	],
	print_board(Board),
	get_all_pieces(Board,white,Pieces),
	get_possible_moves(Board,Pieces,Moves),
	length(Moves,1).

test(get_all_moves_none):-
	print_board([]),
	\+ get_all_moves([],white,_),
	\+ get_all_moves([],black,_).

test(get_all_moves_init_white):-
	initial_board(Board),
	print_board(Board),
	get_all_moves(Board,white,Moves),
	length(Moves,7).

test(get_all_moves_init_black):-
	initial_board(Board),
	print_board(Board),
	get_all_moves(Board,black,Moves),
	length(Moves,7).

test(get_all_moves_only_capture_white):-
	initial_white(White),
	Black = [
		piece(black,man,4,4),
		piece(black,man,6,2)
	],
	append(White,Black,Board),
	print_board(Board),
	get_all_moves(Board,white,Moves),
	length(Moves,2).

test(get_all_moves_only_move_white):-
	initial_white(White),
	Black = [
		piece(black,man,4,4),
		piece(black,man,5,5),
		piece(black,man,5,3)
	],
	append(White,Black,Board),
	print_board(Board),
	get_all_moves(Board,white,Moves),
	length(Moves,5).

test(get_all_moves_only_capture_black):-
	initial_black(Black),
	White = [
		piece(white,man,5,3),
		piece(white,man,3,3),
		piece(white,queen,3,5)
	],
	append(White,Black,Board),
	print_board(Board),
	get_all_moves(Board,black,Moves),
	length(Moves,3).

test(get_all_moves_only_move_black):-
	initial_black(Black),
	White = [
		piece(white,man,5,7),
		piece(white,man,4,8),
		piece(white,man,4,6)
	],
	append(White,Black,Board),
	print_board(Board),
	get_all_moves(Board,black,Moves),
	length(Moves,5).

test(get_all_moves_only_move_black_queen_capture):-
	Board = [
		piece(black,queen,7,1),
		piece(white,man,4,4),
		piece(white,man,3,7)
	],
	print_board(Board),
	get_all_moves(Board,black,Moves),
	length(Moves,3).

test(get_all_moves_no_legal_moves_black):-
	Board = [
		piece(white,man,3,1),
		piece(white,man,2,2),
		piece(white,man,1,3),
		piece(white,man,4,2),
		piece(white,man,3,3),
		piece(white,man,2,4),
		piece(white,man,4,4),
		piece(white,man,5,3),
		piece(white,man,3,5),
		piece(white,man,3,7),
		piece(white,man,2,8),
		piece(white,man,4,8),
		piece(black,man,7,3),
		piece(black,man,7,1),
		piece(black,man,7,5),
		piece(black,man,6,2),
		piece(black,man,6,4),
		piece(black,man,5,1),
		piece(black,man,6,6),
		piece(black,man,6,8),
		piece(black,man,5,5),
		piece(black,man,5,7),
		piece(black,man,4,6)
	],
	print_board(Board),
	\+ get_all_moves(Board,black,_).

test(get_all_moves_no_legal_moves_white):-
	Board = [
		piece(white,man,3,1),
		piece(white,man,2,2),
		piece(white,man,1,3),
		piece(white,man,4,2),
		piece(white,man,3,3),
		piece(white,man,2,4),
		piece(white,man,4,4),
		piece(white,man,5,3),
		piece(white,man,3,5),
		piece(white,man,3,7),
		piece(white,man,2,8),
		piece(white,man,4,8),
		piece(black,man,7,3),
		piece(black,man,7,1),
		piece(black,man,7,5),
		piece(black,man,6,2),
		piece(black,man,6,4),
		piece(black,man,5,1),
		piece(black,man,6,6),
		piece(black,man,6,8),
		piece(black,man,5,5),
		piece(black,man,5,7),
		piece(black,man,4,6)
	],
	print_board(Board),
	\+ get_all_moves(Board,white,_).

test(static_evaluation_1):-
	Board = [
		piece(white,man,3,1),
		piece(white,man,2,2),
		piece(white,man,1,3),
		piece(white,man,4,2),
		piece(white,man,3,3),
		piece(white,man,2,4),
		piece(white,man,4,4),
		piece(white,man,5,3),
		piece(white,man,3,5),
		piece(white,man,3,7),
		piece(white,man,2,8),
		piece(white,man,4,8),
		piece(black,man,7,3),
		piece(black,man,7,1),
		piece(black,man,7,5),
		piece(black,man,6,2),
		piece(black,man,6,4),
		piece(black,man,5,1),
		piece(black,man,6,6),
		piece(black,man,6,8),
		piece(black,man,5,5),
		piece(black,man,5,7),
		piece(black,man,4,6)
	],
	print_board(Board),
	static_evaluation(Board,white,1).

test(static_evaluation_2):-
	Board = [
		piece(white,man,3,1),
		piece(white,man,2,2),
		piece(white,man,1,3),
		piece(white,man,4,2),
		piece(white,man,3,3),
		piece(white,man,2,4),
		piece(white,man,4,4),
		piece(white,man,5,3),
		piece(white,man,3,5),
		piece(white,man,3,7),
		piece(white,man,2,8),
		piece(white,man,4,8),
		piece(black,man,7,3),
		piece(black,man,7,1),
		piece(black,man,7,5),
		piece(black,man,6,2),
		piece(black,man,6,4),
		piece(black,man,5,1),
		piece(black,man,6,6),
		piece(black,man,6,8),
		piece(black,man,5,5),
		piece(black,queen,5,7),
		piece(black,queen,4,6)
	],
	print_board(Board),
	static_evaluation(Board,black,1).

test(minimax_1):-
	Board = [
		piece(white,man,3,3),
		piece(black,man,4,4),
		piece(black,man,6,6),
		piece(black,man,4,2)
	],
	print_board(Board),
	minimax(Board,white,white,[ 
		c(3,3,4,4,5,5,[
			piece(black,man,6,6),
			piece(black,man,4,2),
			piece(white,man,5,5)
		]),
	  	c(5,5,6,6,7,7,[
			  piece(black,man,4,2),
			piece(white,man,7,7)
		])
	],1,0).

test(real_test_case_1, [cleanup(abolish(ruut/3))]):-
	test_case_1,
	board_snapshot(Board),
	print_board(Board),
	choose_move(Board,white,0,0,0,
		m(3,1,4,2,[
			piece(white,man,2,2),
			piece(white,man,2,4),
			piece(white,man,2,8),
			piece(white,man,1,5),
			piece(white,man,3,5),
			piece(white,man,3,7),
			piece(white,man,4,6),
			piece(black,man,6,2),
			piece(black,man,7,3),
			piece(black,man,7,5),
			piece(white,man,4,2)
		])).

test(real_test_case_2, [cleanup(abolish(ruut/3))]):-
	test_case_2,
	board_snapshot(Board),
	print_board(Board),
	choose_move(Board,white,0,0,0,[c(6,4,5,5,4,6,_)|_]).

test(square_promotion_black, [cleanup(abolish(ruut/3))]):-
	assert(ruut(2,2,2)),
	assert(ruut(1,1,0)),
	make_move(2,2,1,1),
	ruut(1,1,20).

test(square_promotion_white, [cleanup(abolish(ruut/3))]):-
	assert(ruut(7,7,1)),
	assert(ruut(8,8,0)),
	make_move(7,7,8,8),
	ruut(8,8,10).

test(simple_move_no_promo, [cleanup(abolish(ruut/3))]):-
	assert(ruut(3,1,1)),
	assert(ruut(4,2,0)),
	make_move(3,1,4,2),
	ruut(4,2,1).

test(check_diagonal_forward_right):-
	Board = [
		piece(white,queen,2,2),
		piece(black,man,6,6)
	],
	check_diagonal(Board,2,2,5,5,1,1).

test(check_diagonal_forward_left):-
	Board = [
		piece(black,queen,3,5),
		piece(black,man,6,2)
	],
	check_diagonal(Board,3,5,5,3,1,-1).

test(check_diagonal_backwards_right):-
	Board = [
		piece(white,queen,7,3),
		piece(black,man,2,8)
	],
	check_diagonal(Board,7,3,3,7,-1,1).

test(check_diagonal_backwards_left):-
	Board = [
		piece(black,queen,6,4),
		piece(white,man,3,1)
	],
	check_diagonal(Board,6,4,4,2,-1,-1).

test(check_diagonal):-
	check_diagonal([],8,8,1,1,-1,-1).

test(check_diagonal_backwards_left):-
	Board = [
		piece(black,queen,6,4),
		piece(white,man,3,1),
		piece(white,man,4,2)
	],
	\+ check_diagonal(Board,6,4,4,2,-1,-1).

test(can_capture_queen_1):-
	Board = [
		piece(black,queen,6,4),
		piece(white,man,4,2)
	],
	can_capture(Board,6,4,-1,4,2,3,1,c(6,4,4,2,3,1,[piece(black,queen,3,1)])).

test(can_capture_queen_2):-
	Board = [
		piece(black,queen,6,4),
		piece(white,man,3,1),
		piece(white,man,4,2)
	],
	\+ can_capture(Board,6,4,-1,4,2,3,1,c(6,4,4,2,3,1,[piece(black,queen,3,1)])).

test(can_capture_queen_3):-
	Board = [
		piece(white,queen,4,4),
		piece(black,man,2,2),
		piece(black,queen,7,7),
		piece(black,man,6,2),
		piece(black,man,2,6)
	],
	findall(Move,can_capture(Board,4,4,1,_,_,_,_,Move),Bag),
	length(Bag,4).

test(can_capture_queen_4):-
	Board = [
		piece(white,queen,4,4),
		piece(black,man,3,3),
		piece(black,queen,7,7),
		piece(black,man,6,2),
		piece(black,man,2,6)
	],
	findall(Move,can_capture(Board,4,4,1,_,_,_,_,Move),Bag),
	length(Bag,5).

test(can_capture_queen_5):-
	Board = [
		piece(black,queen,7,1),
		piece(white,man,4,4),
		piece(black,man,3,7)
	],
	findall(Move,can_capture(Board,7,1,-1,_,_,_,_,Move),Bag),
	length(Bag,3).

test(get_post_positions_white_simple):-
	Board = [
		piece(white,queen,1,1),
		piece(black,man,4,4)
	],
	print_board(Board),
	get_post_positions(Board,4,4,1,1,Positions),
	length(Positions,4).

test(get_post_positions_black_simple):-
	Board = [
		piece(black,queen,6,8),
		piece(white,man,3,5)
	],
	print_board(Board),
	get_post_positions(Board,3,5,-1,-1,Positions),
	length(Positions,2).

test(can_move_queen_white):-
	Board = [
		piece(white,queen,4,4)
	],
	get_all_moves(Board,white,[ 
		m(4,4,5,5,[piece(white,queen,5,5)]),
		m(4,4,6,6,[piece(white,queen,6,6)]),
		m(4,4,7,7,[piece(white,queen,7,7)]),
		m(4,4,8,8,[piece(white,queen,8,8)]),
		m(4,4,3,3,[piece(white,queen,3,3)]),
		m(4,4,2,2,[piece(white,queen,2,2)]),
		m(4,4,1,1,[piece(white,queen,1,1)]),
		m(4,4,3,5,[piece(white,queen,3,5)]),
		m(4,4,2,6,[piece(white,queen,2,6)]),
		m(4,4,1,7,[piece(white,queen,1,7)]),
		m(4,4,5,3,[piece(white,queen,5,3)]),
		m(4,4,6,2,[piece(white,queen,6,2)]),
		m(4,4,7,1,[piece(white,queen,7,1)])
  ]).

test(can_move_queen_black):-
	Board = [
		piece(black,queen,5,5)
	],
	get_all_moves(Board,black,[ 
		m(5,5,6,6,[piece(black,queen,6,6)]),
		m(5,5,7,7,[piece(black,queen,7,7)]),
		m(5,5,8,8,[piece(black,queen,8,8)]),
		m(5,5,4,4,[piece(black,queen,4,4)]),
		m(5,5,3,3,[piece(black,queen,3,3)]),
		m(5,5,2,2,[piece(black,queen,2,2)]),
		m(5,5,1,1,[piece(black,queen,1,1)]),
		m(5,5,4,6,[piece(black,queen,4,6)]),
		m(5,5,3,7,[piece(black,queen,3,7)]),
		m(5,5,2,8,[piece(black,queen,2,8)]),
		m(5,5,6,4,[piece(black,queen,6,4)]),
		m(5,5,7,3,[piece(black,queen,7,3)]),
		m(5,5,8,2,[piece(black,queen,8,2)])
  ]).

test(alpha_beta_1):-
	Board = [
		piece(white,man,3,3),
		piece(black,man,4,4),
		piece(black,man,6,6),
		piece(black,man,4,2)
	],
	print_board(Board),
	alpha_beta(Board,white,white,-1000,1000,Move,Score,0),
	Score =:= 1,
	Move = [c(3,3,4,4,5,5,[
		piece(black,man,6,6),
		piece(black,man,4,2),
		piece(white,man,5,5)
	]),
	  c(5,5,6,6,7,7,[
		  piece(black,man,4,2),
		  piece(white,man,7,7)
	])].

test(real_test_case_3):-
	initial_board(Board),
	choose_move(Board,white,0,0,0,Move).

% test utils
initial_board([ 
	piece(white,man,1,1),
	piece(white,man,1,3),
	piece(white,man,1,5),
	piece(white,man,1,7),
	piece(white,man,2,2),
	piece(white,man,2,4),
	piece(white,man,2,6),
	piece(white,man,2,8),
	piece(white,man,3,1),
	piece(white,man,3,3),
	piece(white,man,3,5),
	piece(white,man,3,7),
	piece(black,man,6,2),
	piece(black,man,6,4),
	piece(black,man,6,6),
	piece(black,man,6,8),
	piece(black,man,7,1),
	piece(black,man,7,3),
	piece(black,man,7,5),
	piece(black,man,7,7),
	piece(black,man,8,2),
	piece(black,man,8,4),
	piece(black,man,8,6),
	piece(black,man,8,8)
]).

initial_white([
	piece(white,man,1,1),
	piece(white,man,1,3),
	piece(white,man,1,5),
	piece(white,man,1,7),
	piece(white,man,2,2),
	piece(white,man,2,4),
	piece(white,man,2,6),
	piece(white,man,2,8),
	piece(white,man,3,1),
	piece(white,man,3,3),
	piece(white,man,3,5),
	piece(white,man,3,7)
]).

initial_black([
	piece(black,man,6,2),
	piece(black,man,6,4),
	piece(black,man,6,6),
	piece(black,man,6,8),
	piece(black,man,7,1),
	piece(black,man,7,3),
	piece(black,man,7,5),
	piece(black,man,7,7),
	piece(black,man,8,2),
	piece(black,man,8,4),
	piece(black,man,8,6),
	piece(black,man,8,8)
]).

init_squares:-
	abolish(ruut/3),
	% white
	assert(ruut(1,1,1)),
	assert(ruut(1,3,1)),
	assert(ruut(1,5,1)),
	assert(ruut(1,7,1)),
	assert(ruut(2,2,1)),
	assert(ruut(2,4,1)),
	assert(ruut(2,6,1)),
	assert(ruut(2,8,1)),
	assert(ruut(3,1,1)),
	assert(ruut(3,3,1)),
	assert(ruut(3,5,1)),
	assert(ruut(3,7,1)),
	% empty
	assert(ruut(4,2,0)),
	assert(ruut(4,4,0)),
	assert(ruut(4,6,0)),
	assert(ruut(4,8,0)),
	assert(ruut(5,1,0)),
	assert(ruut(5,3,0)),
	assert(ruut(5,5,0)),
	assert(ruut(5,7,0)),
	% black
	assert(ruut(6,2,2)),
	assert(ruut(6,4,2)),
	assert(ruut(6,6,2)),
	assert(ruut(6,8,2)),
	assert(ruut(7,1,2)),
	assert(ruut(7,3,2)),
	assert(ruut(7,5,2)),
	assert(ruut(7,7,2)),
	assert(ruut(8,2,2)),
	assert(ruut(8,4,2)),
	assert(ruut(8,6,2)),
	assert(ruut(8,8,2)).

test_case_1:-
	abolish(ruut/3),
	% white
	assert(ruut(3,1,1)),
	assert(ruut(2,2,1)),
	assert(ruut(2,4,1)),
	assert(ruut(2,8,1)),
	assert(ruut(1,5,1)),
	assert(ruut(3,5,1)),
	assert(ruut(3,7,1)),
	assert(ruut(4,6,1)),
	% empty
	assert(ruut(1,1,0)),
	assert(ruut(1,7,0)),
	assert(ruut(2,6,0)),
	assert(ruut(3,1,0)),
	assert(ruut(3,3,0)),
	assert(ruut(4,2,0)),
	assert(ruut(4,4,0)),
	assert(ruut(4,8,0)),
	assert(ruut(5,1,0)),
	assert(ruut(5,3,0)),
	assert(ruut(5,5,0)),
	assert(ruut(5,7,0)),
	assert(ruut(6,4,0)),
	assert(ruut(6,6,0)),
	assert(ruut(6,8,0)),
	assert(ruut(7,1,0)),
	assert(ruut(7,7,0)),
	assert(ruut(8,2,0)),
	assert(ruut(8,4,0)),
	assert(ruut(8,6,0)),
	assert(ruut(8,8,0)),
	% black
	assert(ruut(6,2,2)),
	assert(ruut(7,3,2)),
	assert(ruut(7,5,2)).

test_case_2:-
	abolish(ruut/3),
	% white
	assert(ruut(5,1,1)),
	assert(ruut(4,2,1)),
	assert(ruut(3,3,1)),
	assert(ruut(2,4,1)),
	assert(ruut(2,6,1)),
	assert(ruut(2,8,1)),
	assert(ruut(6,4,1)),
	assert(ruut(8,4,10)),

	% empty
	assert(ruut(3,1,0)),
	assert(ruut(2,2,0)),
	assert(ruut(1,5,0)),
	assert(ruut(3,5,0)),
	assert(ruut(3,7,0)),
	assert(ruut(4,6,0)),
	assert(ruut(1,1,0)),
	assert(ruut(1,7,0)),
	assert(ruut(3,1,0)),
	assert(ruut(4,4,0)),
	assert(ruut(4,8,0)),
	assert(ruut(5,3,0)),
	assert(ruut(6,6,0)),
	assert(ruut(6,8,0)),
	assert(ruut(7,1,0)),
	assert(ruut(7,7,0)),
	assert(ruut(8,2,0)),
	assert(ruut(8,6,0)),
	assert(ruut(8,8,0)),
	assert(ruut(6,2,0)),
	assert(ruut(7,5,0)),
	assert(ruut(7,3,0)),
	% black
	assert(ruut(5,5,2)),
	assert(ruut(5,7,2)).

:- end_tests(iaib200582).

