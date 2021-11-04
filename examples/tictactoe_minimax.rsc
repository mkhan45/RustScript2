let repeat(x, n) = {
    let helper(x, n, acc) = match n
	| 0 -> acc
	| n -> helper(x, n - 1, [x | acc])

    helper(x, n, [])
}

let empty_board() = repeat(:empty, 9)

let square_to_string(square) = match square
    | :empty -> " "
    | :x     -> "X"
    | :o     -> "O"

let nth(ls, i) = match i
    | 0 -> ^ls
    | i -> nth($ls, i - 1)

let set_nth(ls, i, x) = match i
    | 0 -> [x | $ls]
    | i -> [^ls | set_nth($ls, i - 1, x)]

let print_board(board) = {
    foreach([0..3], fn(i) => {
	let [a, b, c] = slice(board, 3 * i, 3 * i + 3)
	let (a, b, c) = (square_to_string(a), square_to_string(b), square_to_string(c))
	println(" " + a + " | " + b + " | " + c)
	if i != 2 then println("-----------") else ()
    })
}

let switch_turn(turn) = match turn
    | :x -> :o
    | :o -> :x

let get_rows(board) = [slice(board, 3 * i, 3 * i + 3) for i in [0..3]]
let get_cols(board) = [[nth(board, 3 * i + j) for i in [0..3]] for j in [0..3]]
let get_diags(board) = [[nth(board, i) for i in [0, 4, 8]], [nth(board, i) for i in [2, 4, 6]]]
let get_sets(board) = flatten([get_set(board) for get_set in [get_rows, get_cols, get_diags]])

let is_winner(board, turn) = {
    let flatten(ls) = fold([], fn(a, b) => a + b, ls)

    let sets = get_sets(board)

    any([
	all([sq == turn for sq in set])
	for set in sets
    ])
}

let is_tied(board) = !any([sq == :empty for sq in board])

let minimax(board, turn, alpha, beta) = {
    if is_winner(board, turn) then {
	(1, board)
    } else if is_winner(board, switch_turn(turn)) then {
	(-1, board)
    } else if !any([sq == :empty for sq in board]) then {
	(0, board)
    } else {
	let possible_moves = [set_nth(board, i, turn) for (i, p) in enumerate(board) if p == :empty]
	let opposite_turn = switch_turn(turn)

	let loop = fn(possible_moves, best_move, alpha, beta) => match possible_moves
	    | [] -> (alpha, best_move)
	    | [nboard | possible_moves] -> {
		let score = alpha
		
		let (score, nmove) = minimax(nboard, opposite_turn, -beta, -alpha)
		let (score, nmove) = if (alpha < score) && (score < beta) then {
		    minimax(nboard, opposite_turn, -beta, -score)
		} else {
		    (score, nmove)
		}

		let score = -score

		if score > alpha then
		    loop(possible_moves, nboard, score, beta)
		else if alpha >= beta then
		    (score, nboard)
		else
		    loop(possible_moves, best_move, alpha, beta)
	    }
	
	loop(possible_moves, ^possible_moves, -999, 999)
    }
}

let ai_move(board, turn) = {
    let num_filled = length([sq for sq in board if sq == :empty])
    if (num_filled == 0) || ((num_filled == 1) && (nth(board, 4) == :empty)) then {
	4
    } else if num_filled < 3 then {
	let sets = get_sets(board)
	let set_indices = get_sets([0..9])
	let zip_inner = fn(xs, ys) => match (xs, ys)
	    | ([], []) -> []
	    | ([x | xs], [y | ys]) -> [zip_rev(x, y) | zip_inner(xs, ys)]

	let indexed_sets = zip(set_indices, sets)

	let opposite_turn = switch_turn(turn)

	# find the missing position if a set is missing only one
	let loop = fn(sets) => match sets
	    | [] -> ()
	    | [set | sets] -> {
		if length([i for (i, sq) in set if sq == opposite_turn]) == 2 then {
		    let (missing_i, missing_sq) = ^[(i, sq) for (i, sq) in set if sq != opposite_turn]
		    if missing_sq == :empty then
			missing_i
		    else
			loop(sets)
		} else {
		    loop(sets)
		}
	    }
	
	let block_move = loop(indexed_sets)
	if block_move != () then {
	    set_nth(board, block_move, turn)
	} else {
	    let corners = [0, 2, 6, 8]
	    let corner_move = find(fn(sq) => sq == :empty, corners)
	    set_nth(board, corner_move, turn)
	}

    } else {
	let (_, new_board) = minimax(board, turn, -999999, 999999)
	new_board
    }
}

let loop(board, turn, player) = {
    println("===========\n")
    print_board(board)

    let get_position = fn() => match string_to_num(scanln())
	| (:ok, n) when nth(board, n) != :empty -> {
	    print("That position is already taken, enter another: ")
	    get_position()
	}
	| (:ok, n) when (0 <= n) && (n <= 8) -> n
	| _ -> {
	    print("Error: position must be a number from 0 to 9: ")
	    get_position()
	}

    let new_board = if turn == player then {
	print("\nChoose a position for " + square_to_string(turn) + ": ")
	let position = get_position()
	set_nth(board, position, player)
    } else {
	ai_move(board, turn)
	#let (_, ai_board) = minimax(board, turn, -999999, 999999)
	#ai_board
    }
	
    if is_winner(new_board, turn) then {
	println(square_to_string(turn) + " Wins!")
	print_board(new_board)
    } else if is_tied(new_board) then {
	println("You tied!")
	print_board(new_board)
    } else {
	loop(new_board, switch_turn(turn), player)
    }
}

println("here")
let board = empty_board()
#let board = set_nth(board, 4, :x)
#let board = set_nth(board, 0, :o)
loop(board, :x, :x)
#let board = empty_board()
#print_board(board)
#let (_, board) = minimax(board, :x, :x)
#println("===========")
#print_board(board)
