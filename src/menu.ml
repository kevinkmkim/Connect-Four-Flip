let print_menu () =
  print_endline "[MENU]";
  print_endline "1) START";
  print_endline "2) LOAD GAME";
  print_endline "3) HIGH SCORES";
  print_endline "4) CONTROLS";
  print_endline "5) EXIT";
  print_string "> "

let rec get_yn () =
  print_string "> ";
  let response = String.lowercase_ascii (read_line ()) in
  if response = "y" || response = "n" then response
  else (
    print_endline "Please enter a valid response";
    get_yn ())

let rec get_input () =
  print_string "> ";
  let value = read_line () in
  if
    (try
       ignore (int_of_string value);
       true
     with _ -> false)
    && int_of_string value > 3
    && int_of_string value < 10
  then value
  else (
    print_endline "Please enter a valid number";
    get_input ())

let set_board_size () =
  print_endline "Would you like to customize the board size? [Y/N]";
  let response = get_yn () in
  if response = "y" then (
    print_endline
      "[Initialize the size of your board]\n\
       *There must be at least 4 rows and 4 columns*\n";
    print_endline "Please initialize the number of rows in your board.";
    let row_num = get_input () in
    print_endline
      "Please initialize the number of columns in your board.";
    let col_num = get_input () in
    print_endline
      ("Board size initialized to (" ^ row_num ^ " x " ^ col_num ^ ")\n");
    (int_of_string row_num, int_of_string col_num))
  else (
    print_endline "Board size initialized to (6 x 7)\n";
    (6, 7))

let rec get_difficulty () =
  print_string "> ";
  let difficulty = int_of_string (read_line ()) in
  if difficulty >= 1 && difficulty <= 3 then difficulty
  else (
    print_endline "Please enter a valid number";
    get_difficulty ())

let set_game_mode () =
  print_endline "Would you like to play against a dumb (random) AI? [Y/N]";
  let response = get_yn () in
  if response = "y" then 1
    (* (print_endline "Select difficulty\n";
    print_endline "1) Easy\n2) Medium\n3) Hard";
    let difficulty = get_difficulty () in
    difficulty) *)
  else -1