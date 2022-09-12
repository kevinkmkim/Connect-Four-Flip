open Board
open State

type t =
  | Insert of int
  | FlipBoard
  | Quit
  | Commands
  | Undo
  | Save of string list

(* exception IndexOutOfBound of int *)
exception InvalidCommand

let is_int s =
  try
    ignore (int_of_string s);
    true
  with _ -> false

let string_to_command str =
  if is_int str then Insert (int_of_string str)
  else
    let str_lst =
      str |> String.split_on_char ' ' |> List.filter (fun x -> x <> "")
    in
    match str_lst with
    | [ "flip" ] -> FlipBoard
    | [ "quit" ] -> Quit
    | [ "undo" ] -> Undo
    | [ "commands" ] -> Commands
    | "save" :: name -> Save name
    | _ -> raise InvalidCommand

(* let rec flip_columns = function | [] -> [] | h :: t -> {index =
   h.index; empty = h.empty; filled = (List.rev h.filled)} ::
   flip_columns t

   let flip_board (board : Board.t) = flip_columns board.start_board *)

(* let make_move () = print_endline "Make your move by inserting a token
   or flipping the board.\nType \"quit\" to quit game"; print_string ">
   "; let commmand = read_line () |> string_to_command in match commmand
   with | Insert x -> Board.place_token | FlipBoard -> flip_board
   current_state | Quit -> "quit" *)

(* let insert_token index state = if state.turn then *)
