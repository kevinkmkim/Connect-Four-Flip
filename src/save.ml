open Yojson.Basic.Util
open State
open Board

(* Exceptions *)
exception InvalidFilename

(* Saving a game into a file *)

let rec lst_to_json lst acc =
  let sep = "          " in
  let sep2 = "        " in
  match lst with
  | [] -> "[]"
  | [ h ] -> acc ^ (sep ^ "\"" ^ h ^ "\"") ^ "\n" ^ sep2 ^ "]"
  | h :: t -> lst_to_json t (acc ^ sep ^ "\"" ^ h ^ "\",\n")

let rec lst_to_json2 lst acc =
  let sep = "            " in
  let sep2 = "          " in
  match lst with
  | [] -> "[]"
  | [ h ] -> acc ^ (sep ^ "\"" ^ h ^ "\"") ^ "\n" ^ sep2 ^ "]"
  | h :: t -> lst_to_json2 t (acc ^ sep ^ "\"" ^ h ^ "\",\n")

let print_col col =
  let sep = "        " in
  "\n" ^ sep ^ "\"index\": "
  ^ string_of_int (get_index col)
  ^ ",\n" ^ sep ^ "\"empty\": "
  ^ lst_to_json (get_empty col) "[\n"
  ^ ",\n" ^ sep ^ "\"filled\": "
  ^ lst_to_json (get_filled col) "[\n"

let print_col2 col =
  let sep = "          " in
  "\n" ^ sep ^ "\"index\": "
  ^ string_of_int (get_index col)
  ^ ",\n" ^ sep ^ "\"empty\": "
  ^ lst_to_json2 (get_empty col) "[\n"
  ^ ",\n" ^ sep ^ "\"filled\": "
  ^ lst_to_json2 (get_filled col) "[\n"

let state_to_string state =
  let sep = "    " in
  let board_layout =
    String.concat "\n      },\n      {"
      (List.map print_col (get_board state))
  in
  sep ^ "\"turn\": "
  ^ string_of_bool (get_turn state)
  ^ ",\n" ^ sep ^ "\"move\": "
  ^ string_of_int (get_move state)
  ^ ",\n" ^ sep ^ "\"board\": [" ^ "\n      {" ^ board_layout ^ sep
  ^ "\n" ^ "      " ^ "}" ^ "\n" ^ sep ^ "]"

let history_to_string state =
  let sep = "      " in
  let board_layout =
    String.concat "\n        },\n        {"
      (List.map print_col2 (get_board state))
  in
  sep ^ "\"turn\": "
  ^ string_of_bool (get_turn state)
  ^ ",\n" ^ sep ^ "\"move\": "
  ^ string_of_int (get_move state)
  ^ ",\n" ^ sep ^ "\"board\": [" ^ "\n        {" ^ board_layout ^ sep
  ^ "\n" ^ "        " ^ "}" ^ "\n" ^ sep ^ "]"

let check_filename namelst =
  if
    List.length namelst <> 1
    || List.hd namelst = ""
    || List.hd namelst = "quit"
  then ".."
  else
    let regex = Str.regexp {|[^a-zA-Z0-9 -]|} in
    let string = Str.global_replace regex "" (List.hd namelst) in
    string

let save_file state history namelst =
  let filename = check_filename namelst in
  if filename = ".." then raise InvalidFilename
  else
    let file = "game_files/" ^ filename ^ ".json" in
    try
      let _ =
        let oc = open_out file in
        Printf.fprintf oc "%s\n" "{";
        Printf.fprintf oc "%s\n" "  \"state\": {";
        Printf.fprintf oc "%s\n" (state_to_string state);
        Printf.fprintf oc "%s\n" "  },";
        Printf.fprintf oc "%s\n" "  \"history\": [\n    {";
        let history_states =
          String.concat "\n    },\n    {\n"
            (List.map history_to_string history)
        in
        Printf.fprintf oc "%s\n" (history_states ^ "\n      }");
        Printf.fprintf oc "%s\n" "  ]";
        Printf.fprintf oc "%s\n" "}";
        close_out oc
      in
      true
    with _ -> false

(* Loading a file into the game *)

let empty_json json = json

let column_json json : Board.column =
  {
    index = json |> member "index" |> to_int;
    empty = json |> member "empty" |> to_list |> filter_string;
    filled = json |> member "filled" |> to_list |> filter_string;
  }

let state_json json : State.t =
  {
    turn = json |> member "turn" |> to_bool;
    move = json |> member "move" |> to_int;
    board = json |> member "board" |> to_list |> List.map column_json;
  }

let from_json json =
  ( json |> member "state" |> state_json,
    json |> member "history" |> to_list |> List.map state_json )

let open_file f =
  if Sys.file_exists f then from_json (Yojson.Basic.from_file f)
  else raise InvalidFilename
