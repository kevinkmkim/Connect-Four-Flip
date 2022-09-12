open Board
open State
open Command
open Random

let rec find_empty_cols board : int list =
  match board with
  | [] -> []
  | h :: t -> if h |> get_empty |> List.length |> (=) 0 then find_empty_cols t
  else get_index h :: find_empty_cols t

let get_random_move state = 
  let possible_cols = state |> get_board |> find_empty_cols in
  let possible_moves = 0 :: possible_cols in
    let move = List.nth possible_moves (Random.int (List.length possible_moves)) in
    if move = 0
      then FlipBoard
    else 
      Insert move
(* let get_best_move =  *)

let move_to_string command = 
  match command with
  | Insert x -> "inserted token in column " ^ string_of_int x
  | FlipBoard -> "flipped board"
  | _ -> "This won't be executed"

let get_ai_move state difficulty = 
  if difficulty = 1 
    
    then (let move = get_random_move state in print_endline ("AI " ^ move_to_string move); move)
  else Insert 2