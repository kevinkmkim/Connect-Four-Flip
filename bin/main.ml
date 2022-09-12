open Game
open Board
open Command
open State
open Check
open Save
open Menu
open Ai

(* let congratulate state = 
  (* check who won then congratulate them *)
  (if fst state.counter > snd state.counter then print_endline "Congratulations Player 1 (X) for winning!" else
    print_endline "Congratulations Player 2 (O) for winning!");
  print_endline "Thank you for playing!" *)


let print_commands () = 
  print_endline "Commands you can input:\n";
  print_endline "> flip -> Flips the board upside down 180 degrees";
  print_endline "> quit -> Quits out and exits the game. Unsaved progress will be lost";
  print_endline "> undo -> Undoes the last action. Can be done all the way until the start of the game";
  print_endline "> commands -> lists the all of the possible commands";
  print_endline "> save -> Saves the state of the board as a json";
  print_endline "> load x -> Loads any boardstate from a board state x.json file";
  print_endline "\n"

let update_state_history instruction turn state history =
  match instruction with
  | FlipBoard -> State.flip state history
  | Quit -> print_endline "Thank you for playing!"; exit 0
  | Commands -> print_commands (); (state, history)
  | Save name -> (try if (Save.save_file state history name)
    then (print_endline "Game saved successfully!"; (state, history))
else (print_endline "Game could not be saved."; (state, history))
with
| InvalidFilename -> print_endline "Invalid filename please choose a different name."; (state, history))
  | Undo -> (try State.undo history
with
| NoMoveMade -> print_endline "Please make a move to place a token"; (state, history))
  | Insert x -> (try (State.place_token state (if turn then "X" else "O") x history)
with
| ColumnFull -> print_endline "The column you chose is full. Please choose a different column."; (state, history)
| OutOfIndex -> print_endline "The column you chose is not one of the choices. Please choose a different column."; (state, history))

let rec pvp_game_loop (state : State.t) history =
  let board = get_board state in
  let turn = get_turn state in
  print_board board;
  if is_game_over_pvp (get_board state) 
    then (print_endline "Thank you for playing!"; exit 0) 
  else
    (if turn then
    ANSITerminal.print_string [ ANSITerminal.magenta ]
    "Player 1's Turn (X): Please make your move\n"
  else
    ANSITerminal.print_string [ ANSITerminal.cyan ]
    "Player 2's Turn (O): Please make your move\n");
  print_string "> ";
  let instruction =
    (try
      (read_line () |> Command.string_to_command)
    with
    | InvalidCommand -> print_endline "Invalid input. Please choose something else."; 
    pvp_game_loop state history)
  in
  let new_state_history = update_state_history instruction turn state history
  in pvp_game_loop (fst new_state_history) (snd new_state_history)

let rec pvc_game_loop difficulty (state : State.t) history =
  let board = get_board state in
  print_board board;
  if is_game_over_pvc (get_board state)
    then (print_endline "Thank you for playing!"; exit 0)
  else
    ANSITerminal.print_string [ ANSITerminal.magenta ]
    "Player's Turn (X): Please make your move\n";
  print_string "> ";
  let instruction =
    (try
      (read_line () |> Command.string_to_command)
    with
    | InvalidCommand -> print_endline "Invalid input. Please choose something else."; 
    pvc_game_loop difficulty state history)
  in
  let post_player_state_history = update_state_history instruction true state history
  in 
  post_player_state_history |> fst |> get_board |> print_board;
  if is_game_over_pvc (get_board (fst post_player_state_history))
    then (print_endline "Thank you for playing!"; exit 0)
  else
    ANSITerminal.print_string [ ANSITerminal.cyan ]
    "AI's Turn (O) ";
  let ai_move = get_ai_move (fst post_player_state_history) difficulty
  in
  let post_ai_state_history = update_state_history ai_move false (fst post_player_state_history) history
  in
  pvc_game_loop difficulty (fst post_ai_state_history) (snd post_ai_state_history)

let play_pvp state =
    let _ = pvp_game_loop state [] in
    ()

let play_pvc difficulty state =
    let _ = pvc_game_loop difficulty state [] in
    ()

let go_back_prompt () =
print_endline "To go back to menu press enter";
read_line ()
  
let display_high_scores () =
print_endline "High Scores displayed";
ignore (go_back_prompt ())
  
let start_game () =
  let size = set_board_size () in
  let mode = set_game_mode () in
  if mode = -1 then
    play_pvp (State.init_state (fst size) (snd size))
  else 
    play_pvc mode (State.init_state (fst size) (snd size))
  
let rec load_game () = 
print_endline "Please enter the name of the file you want to load.";
let filename = read_line () in
if filename = "quit" then exit 0 else
  let file_name = "game_files" ^ Filename.dir_sep ^ filename ^ ".json" in
  try (let game = open_file file_name in
        let state = fst game in
        let history = snd game in
  ignore(pvp_game_loop state history)) with
  | InvalidFilename -> print_endline "File not found. Please try again."; load_game ()
  
let display_controls () =
  print_endline "[CONTROLS]
  When prompted, player must type in their move
  * Possible moves:
  - Typing in a column number will insert the players piece into that column
  - Typing 'flip' will flip the board, either 180 degrees or 90 degrees depending on selected game mode
  - Typing 'quit' will quit the current game and return to the menu";
  ignore(go_back_prompt ())
  
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
  "\n\nWelcome to the Connect-4 Flip Game engine.\n";
  print_menu ();
  
  let menu_choice =  read_line () in
  let check = ref (String.lowercase_ascii menu_choice) in
  while !check <> "exit" && !check <> "5"  do
    print_string "\n";
    if !check = "start" || !check = "1" then start_game ()
    else if !check = "load game" || !check = "2" then load_game ()
    else if !check = "high scores" || !check = "3" then display_high_scores ()
    else if !check = "controls" || !check = "4" then display_controls ()
    else print_endline "Bad input";
    print_menu ();
  
    let menu_choice =  read_line () in
    check := String.lowercase_ascii menu_choice;
  done
  
let () = main ()
