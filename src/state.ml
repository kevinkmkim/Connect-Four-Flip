open Board
(* open Command *)

type t = {
  turn : bool;
  board : Board.t;
  move : int;
}

exception NoMoveMade
exception ColumnFull
exception OutOfIndex

let get_turn state = state.turn
let get_board state = state.board
let get_move state = state.move

let init_state row col =
  { turn = true; board = make_board row col; move = 0 }

let rec get_num_cols (board : Board.t) =
  match board with
  | [] -> 0
  | _ :: t -> 1 + get_num_cols t

let place_token state token col history =
  let board = get_board state in
  let player = get_turn state in
  let move = get_move state in
  if col < 1 || col > get_num_cols board then raise OutOfIndex
  else
    let rec place current : Board.t =
      match current with
      | [] -> []
      | h :: t ->
          if get_index h = col then
            if get_empty h = [] then raise ColumnFull
            else
              [
                make_column (get_index h)
                  (Board.fill_empty (List.length (get_empty h) - 1))
                  ([ token ] @ get_filled h);
              ]
              @ place t
          else [ h ] @ place t
    in
    let new_state =
      { turn = not player; board = place board; move = move + 1 }
    in
    (new_state, state :: history)

let flip state history =
  let board = get_board state in
  let player = get_turn state in
  let move = get_move state in
  let rec flip_rec board =
    match board with
    | [] -> []
    | h :: t ->
        [
          make_column (get_index h) (get_empty h)
            (List.rev (get_filled h));
        ]
        @ flip_rec t
  in
  let new_state =
    { turn = not player; board = flip_rec board; move = move + 1 }
  in
  (new_state, state :: history)

let undo history =
  try (List.hd history, List.tl history)
  with Failure e ->
    ignore e;
    raise NoMoveMade
