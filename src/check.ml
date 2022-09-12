open Board

let rec ( -- ) i j = if i >= j then [] else i :: (i + 1 -- j)

let rec left_rect_range = function
  | 3 -> []
  | x ->
      ((5 * (x - 4)) -- ((5 * (x - 4)) + 2)) :: left_rect_range (x - 1)

let rec right_rect_range = function
  | 3 -> []
  | x -> ((5 * (x - 3)) - 2 -- (5 * (x - 3))) :: right_rect_range (x - 1)

let left_rect height = height |> left_rect_range |> List.flatten
let right_rect height = height |> right_rect_range |> List.flatten

let right_diag_fours x =
  [ x; x + (5 * 1) + 1; x + (5 * 2) + 2; x + (5 * 3) + 3 ]

let left_diag_fours x =
  [ x; x + (5 * 1) - 1; x + (5 * 2) - 2; x + (5 * 3) - 3 ]

let right_diag_fours_list height =
  height |> left_rect |> List.map right_diag_fours

let left_diag_fours_list height =
  height |> right_rect |> List.map left_diag_fours

let indices_to_strings string_list = List.map (List.nth string_list)

let board_full (board : Board.t) =
  board
  |> List.map (fun col -> List.length (get_empty col) = 0)
  |> List.for_all (fun x -> x = true)

(* let game_over board = let p1_score = number_of_fours "X" in let
   p2_score = number_of_fours "O" in if board_full board then
   print_endline "The board is full. It's a tie!"; true else if p1_score
   > p2_score then print_endline "Congratulationas Player 1. You won!";
   true else if p1_score < p2_score then print_endline "Congratulationas
   Player 2. You won!"; true else false *)

(* ---------------------------------------------------- *)

let board_list_rep (board : string list list) : string list =
  List.flatten board

let board_height (board : string list list) = List.length board

let board_width board =
  let total = List.length (board_list_rep board) in
  let height = board_height board in
  total / height

let ur_conds pos width : bool =
  pos > 3 * width && pos mod width <= width - 3 && 0 < pos mod width

let ur_check (board_lst : string list) pos width : string =
  let pos1 = List.nth board_lst pos in
  let pos2 = List.nth board_lst (pos - width + 1) in
  let pos3 = List.nth board_lst (pos - (2 * width) + 2) in
  let pos4 = List.nth board_lst (pos - (3 * width) + 3) in
  let lst = [ pos1; pos2; pos3; pos4 ] in
  if
    List.for_all (fun x -> x = "X") lst
    || List.for_all (fun x -> x = "O") lst
  then List.nth board_lst pos
  else "Z"

let ul_conds pos width : bool =
  pos > 3 * width && (pos mod width > 3 || pos mod width == 0)

let ul_check (board_lst : string list) pos width : string =
  let pos1 = List.nth board_lst pos in
  let pos2 = List.nth board_lst (pos - width - 1) in
  let pos3 = List.nth board_lst (pos - (2 * width) - 2) in
  let pos4 = List.nth board_lst (pos - (3 * width) - 3) in
  let lst = [ pos1; pos2; pos3; pos4 ] in
  if
    List.for_all (fun x -> x = "X") lst
    || List.for_all (fun x -> x = "O") lst
  then List.nth board_lst pos
  else "Z"

let diag_check (board : string list list) =
  let board_lst = board_list_rep board in
  let width = board_width board in
  let size = List.length board_lst - 1 in
  let acc = ref [] in
  for i = 0 to size do
    if ur_conds i width then acc := ur_check board_lst i width :: !acc;
    if ul_conds i width then acc := ul_check board_lst i width :: !acc
  done;
  List.filter (fun x -> x <> "Z") !acc

let diag_check_parse lst =
  let numX = List.length (List.filter (fun x -> x = "X") lst) in
  let numO = List.length (List.filter (fun x -> x = "O") lst) in
  (numX, numO)
(* if numX > numO then "X" else if numX < numO then "O" else "tie" *)

(* --------------------------------------------------------------------- *)

let up_conds pos width : bool = pos > 3 * width

let up_check (board_lst : string list) pos width : string =
  let pos1 = List.nth board_lst pos in
  let pos2 = List.nth board_lst (pos - width) in
  let pos3 = List.nth board_lst (pos - (2 * width)) in
  let pos4 = List.nth board_lst (pos - (3 * width)) in
  let lst = [ pos1; pos2; pos3; pos4 ] in
  if
    List.for_all (fun x -> x = "X") lst
    || List.for_all (fun x -> x = "O") lst
  then List.nth board_lst pos
  else "Z"

let vert_check (board : string list list) =
  let board_lst = board_list_rep board in
  let width = board_width board in
  let size = List.length board_lst - 1 in
  let acc = ref [] in
  for i = 0 to size do
    if up_conds i width then acc := up_check board_lst i width :: !acc
  done;
  List.filter (fun x -> x <> "Z") !acc

let vert_check_parse lst =
  let numX = List.length (List.filter (fun x -> x = "X") lst) in
  let numO = List.length (List.filter (fun x -> x = "O") lst) in
  (numX, numO)
(* if numX > numO then "X" else if numX < numO then "O" else "tie" *)

(* --------------------------------------------------------------------- *)

let rec list_to_string (lst : string list) : string =
  match lst with
  | [] -> ""
  | h :: t -> h ^ list_to_string t

(* https://stackoverflow.com/questions/11193783/ocaml-strings-and-substrings *)
let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let hori_check (board : string list list) =
  let board_T = transpose board in
  board_T |> vert_check |> vert_check_parse

(* --------------------------------------------------------------------- *)

let score_tuple board =
  let matrix = cols_to_matrix board in
  let diag_lst = diag_check matrix in
  let diag_scores = diag_check_parse diag_lst in
  let hori_scores = hori_check matrix in
  let vert_lst = vert_check matrix in
  let vert_scores = vert_check_parse vert_lst in
  let p1_score = fst diag_scores + fst hori_scores + fst vert_scores in
  let p2_score = snd diag_scores + snd hori_scores + snd vert_scores in
  (p1_score, p2_score)

let print_scores_pvp p1_score p2_score =
  if p1_score > 1 && p2_score > 1 then (
    print_endline
      ("Player 1 has " ^ string_of_int p1_score ^ " connect-four's");
    print_endline
      ("Player 2 has " ^ string_of_int p2_score ^ " connect-four's"))
  else if p1_score > 1 && p2_score < 1 then (
    print_endline
      ("Player 1 has " ^ string_of_int p1_score ^ " connect-four's");
    print_endline
      ("Player 2 has " ^ string_of_int p2_score ^ " connect-four"))
  else if p1_score < 1 && p2_score > 1 then (
    print_endline
      ("Player 1 has " ^ string_of_int p1_score ^ " connect-four");
    print_endline
      ("Player 2 has " ^ string_of_int p2_score ^ " connect-four's"))
  else (
    print_endline
      ("Player 1 has " ^ string_of_int p1_score ^ " connect-four");
    print_endline
      ("Player 2 has " ^ string_of_int p2_score ^ " connect-four"))

let print_scores_pvc p1_score p2_score =
  if p1_score > 1 && p2_score > 1 then (
    print_endline
      ("Player has " ^ string_of_int p1_score ^ " connect-four's");
    print_endline
      ("AI has " ^ string_of_int p2_score ^ " connect-four's"))
  else if p1_score > 1 && p2_score < 1 then (
    print_endline
      ("Player has " ^ string_of_int p1_score ^ " connect-four's");
    print_endline ("AI has " ^ string_of_int p2_score ^ " connect-four"))
  else if p1_score < 1 && p2_score > 1 then (
    print_endline
      ("Player has " ^ string_of_int p1_score ^ " connect-four");
    print_endline
      ("AI has " ^ string_of_int p2_score ^ " connect-four's"))
  else (
    print_endline
      ("Player has " ^ string_of_int p1_score ^ " connect-four");
    print_endline
      ("AI has " ^ string_of_int p2_score ^ " connect-four"))

let is_game_over_pvp (board : column list) : bool =
  let scores = score_tuple board in
  let p1_score = fst scores in
  let p2_score = snd scores in
  if p1_score > p2_score then (
    print_scores_pvp p1_score p2_score;
    print_endline "\n[Player 1 won]\n";
    true)
  else if p1_score < p2_score then (
    print_scores_pvp p1_score p2_score;
    print_endline "\n[Player 2 won]\n";
    true)
  else false

let is_game_over_pvc (board : column list) : bool =
  let scores = score_tuple board in
  let player_score = fst scores in
  let ai_score = snd scores in
  if player_score > ai_score then (
    print_scores_pvc player_score ai_score;
    print_endline "\n[You won]\n";
    true)
  else if player_score < ai_score then (
    print_scores_pvc player_score ai_score;
    print_endline "\n[You lost]\n";
    true)
  else false
