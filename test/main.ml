open OUnit2
open Game
open Board
open Check
open State
open Command

(* open Menu *)
open Save

(*****************************************************************)
(* Unit tests *)
(*****************************************************************)

(** [make_column_test index empty expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [empty]. *)
let make_column_test
    (name : string)
    (index : int)
    (empty : string list)
    (filled : string list)
    (expected_output : column) : test =
  name >:: fun _ ->
  assert_equal expected_output (make_column index empty filled)

let make_board_test
    (name : string)
    (row : int)
    (col : int)
    (expected_output : column list) : test =
  name >:: fun _ -> assert_equal expected_output (make_board row col)

let fill_empty_test
    (name : string)
    (length : int)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (fill_empty length)

let cols_to_matrix_test
    (name : string)
    (columns : column list)
    (expected_output : string list list) : test =
  name >:: fun _ ->
  assert_equal expected_output (cols_to_matrix columns)

(* End of test functions for [Board] module *)

let right_diag_fours_test
    (name : string)
    (x : int)
    (expected_output : int list) : test =
  name >:: fun _ -> assert_equal expected_output (right_diag_fours x)

let left_diag_fours_test
    (name : string)
    (x : int)
    (expected_output : int list) : test =
  name >:: fun _ -> assert_equal expected_output (left_diag_fours x)

let indices_to_strings_test
    (name : string)
    (string_list : string list)
    (int_list : int list)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (indices_to_strings string_list int_list)

let score_tuple_test
    (name : string)
    (board : column list)
    (expected_output : int * int) : test =
  name >:: fun _ -> assert_equal expected_output (score_tuple board)

let is_game_over_pvp_test
    (name : string)
    (board : column list)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (is_game_over_pvp board)

let is_game_over_pvc_test
    (name : string)
    (board : column list)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (is_game_over_pvc board)

(* End of test functions for [Check] module *)

let init_state_test
    (name : string)
    (rows : int)
    (cols : int)
    (expected_output : State.t) : test =
  name >:: fun _ -> assert_equal expected_output (init_state rows cols)

let place_token_test
    (name : string)
    (state : State.t)
    (token : string)
    (index : int)
    (history : State.t list)
    (expected_output : State.t * State.t list) : test =
  name >:: fun _ ->
  assert_equal expected_output (place_token state token index history)

let place_token_error_test
    (name : string)
    (state : State.t)
    (token : string)
    (index : int)
    (history : State.t list)
    (expected_output : exn) : test =
  let f _ = place_token state token index history in
  name >:: fun _ -> assert_raises expected_output f

let flip_test
    (name : string)
    (state : State.t)
    (history : State.t list)
    (expected_output : State.t * State.t list) : test =
  name >:: fun _ -> assert_equal expected_output (flip state history)

let undo_test
    (name : string)
    (history : State.t list)
    (expected_output : State.t * State.t list) : test =
  name >:: fun _ -> assert_equal expected_output (undo history)

let undo_error_test
    (name : string)
    (history : State.t list)
    (expected_output : exn) : test =
  let f _ = undo history in
  name >:: fun _ -> assert_raises expected_output f

(* End of test functions for [State] module *)

let string_to_command_test
    (name : string)
    (command_string : string)
    (expected_output : Command.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (string_to_command command_string)

let string_to_command_error_test
    (name : string)
    (command_string : string)
    (expected_output : exn) : test =
  let f _ = string_to_command command_string in
  name >:: fun _ -> assert_raises expected_output f

(* End of test function for [Command] module *)

let save_file_test
    (name : string)
    (state : State.t)
    (history : State.t list)
    (filename : string list)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (save_file state history filename)

let save_file_error_test
    (name : string)
    (state : State.t)
    (history : State.t list)
    (filename : string list)
    (expected_output : exn) : test =
  let f _ = save_file state history filename in
  name >:: fun _ -> assert_raises expected_output f

let open_file_test
    (name : string)
    (filename : string)
    (expected_output : State.t * State.t list) : test =
  name >:: fun _ -> assert_equal expected_output (open_file filename)

let open_file_error_test
    (name : string)
    (filename : string)
    (expected_output : exn) : test =
  let f _ = open_file filename in
  name >:: fun _ -> assert_raises expected_output f

(* End of test function for [Save] module *)

let make_column_tests =
  [
    make_column_test "make_column_test 1" 1 [] [ "X" ]
      { index = 1; empty = []; filled = [ "X" ] };
    make_column_test "make_column_test 2" 1 [] [ "X"; "O" ]
      { index = 1; empty = []; filled = [ "X"; "O" ] };
  ]

(* Helper function that generates [x] number of empty spaces in the
   board *)
let empty_spaces x = List.init x (fun _ -> "_")

let make_board_tests =
  [
    make_board_test "make_board_test 3x3" 3 3
      [
        { index = 1; empty = empty_spaces 3; filled = [] };
        { index = 2; empty = empty_spaces 3; filled = [] };
        { index = 3; empty = empty_spaces 3; filled = [] };
      ];
    make_board_test "make_board_test 2x2" 2 2
      [
        { index = 1; empty = (empty_spaces 2); filled = [] };
        { index = 2; empty = (empty_spaces 2); filled = [] };
      ];
  ]

let fill_empty_tests =
  [
    fill_empty_test "fill_empty_test size 0" 0 [];
    fill_empty_test "fill_empty_test size 2" 2 (empty_spaces 2);
    fill_empty_test "fill_empty_test size 3" 3 (empty_spaces 3);
  ]

let cols_to_matrix_tests =
  [
    cols_to_matrix_test "cols_to_matrix_test filled is empty"
      [ { index = 1; empty = (empty_spaces 3); filled = [] } ]
      [ (empty_spaces 3) ];
    cols_to_matrix_test "cols_to_matrix_test empty is empty"
      [
        { index = 1; empty = (empty_spaces 3); filled = [] };
        { index = 2; empty = []; filled = [ "X"; "O"; "X" ] };
      ]
      [ (empty_spaces 3); [ "X"; "O"; "X" ] ];
    cols_to_matrix_test "cols_to_matrix_test both are partially filled"
      [
        { index = 1; empty = (empty_spaces 2); filled = [ "O" ] };
        { index = 2; empty = (empty_spaces 2); filled = [ "X" ] };
        { index = 3; empty = [ "_" ]; filled = [ "O"; "X" ] };
      ]
      [ [ "_"; "_"; "O" ]; [ "_"; "_"; "X" ]; [ "_"; "O"; "X" ] ];
  ]

let board_tests =
  make_column_tests @ make_board_tests @ fill_empty_tests
  @ cols_to_matrix_tests

let right_diag_fours_tests =
  [
    right_diag_fours_test "right_diag_fours test 1" 0 [ 0; 6; 12; 18 ];
    right_diag_fours_test "right_diag_fours test 2" 1 [ 1; 7; 13; 19 ];
    right_diag_fours_test "right_diag_fours test 3" 2 [ 2; 8; 14; 20 ];
    right_diag_fours_test "right_diag_fours test 4" 3 [ 3; 9; 15; 21 ];
    right_diag_fours_test "right_diag_fours test 5" 4 [ 4; 10; 16; 22 ];
    right_diag_fours_test "right_diag_fours test 6" 100
      [ 100; 106; 112; 118 ];
    right_diag_fours_test "right_diag_fours test 7" 101
      [ 101; 107; 113; 119 ];
    right_diag_fours_test "right_diag_fours test 8" 102
      [ 102; 108; 114; 120 ];
    right_diag_fours_test "right_diag_fours test 9" 103
      [ 103; 109; 115; 121 ];
    right_diag_fours_test "right_diag_fours test 10" 104
      [ 104; 110; 116; 122 ];
  ]

let left_diag_fours_tests =
  [
    left_diag_fours_test "left_diag_fours test 1" 0 [ 0; 4; 8; 12 ];
    left_diag_fours_test "left_diag_fours test 2" 1 [ 1; 5; 9; 13 ];
    left_diag_fours_test "left_diag_fours test 3" 2 [ 2; 6; 10; 14 ];
    left_diag_fours_test "left_diag_fours test 4" 3 [ 3; 7; 11; 15 ];
    left_diag_fours_test "left_diag_fours test 5" 4 [ 4; 8; 12; 16 ];
    left_diag_fours_test "left_diag_fours test 6" 100
      [ 100; 104; 108; 112 ];
    left_diag_fours_test "left_diag_fours test 7" 101
      [ 101; 105; 109; 113 ];
    left_diag_fours_test "left_diag_fours test 8" 102
      [ 102; 106; 110; 114 ];
    left_diag_fours_test "left_diag_fours test 9" 103
      [ 103; 107; 111; 115 ];
    left_diag_fours_test "left_diag_fours test 10" 104
      [ 104; 108; 112; 116 ];
  ]

let indices_to_strings_tests =
  [
    indices_to_strings_test "indices_to_strings test 1"
      (empty_spaces 24)
      [ 0; 4; 8; 12 ] [ "_"; "_"; "_"; "_" ];
    indices_to_strings_test "indices_to_strings test 2"
      (["o";"x"]@empty_spaces 22)
      [ 0; 4; 8; 12 ] [ "o"; "_"; "_"; "_" ];
    indices_to_strings_test "indices_to_strings test 3"
      (empty_spaces 24)
      [ 1; 5; 9; 13 ] [ "_"; "_"; "_"; "_" ];
    indices_to_strings_test "indices_to_strings test 4"
      (["o";"x"]@ empty_spaces 22)
      [ 1; 5; 9; 13 ] [ "x"; "_"; "_"; "_" ];
    indices_to_strings_test "indices_to_strings test 5"
      (["o";"x"]@ empty_spaces 22)
      [ 2; 8; 14; 20 ] [ "_"; "_"; "_"; "_" ];
  ]

let score_tuple_tests =
  [
    (* [Edge Case 1 ] Board with minimum number of rows and columns *)
    score_tuple_test "score_tuple test 1"
      [
        { index = 1; empty = (empty_spaces 4); filled = [] };
        { index = 2; empty = (empty_spaces 4); filled = [] };
        { index = 3; empty = (empty_spaces 4); filled = [] };
        { index = 4; empty = (empty_spaces 4); filled = [] };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 2"
      [
        { index = 1; empty = (empty_spaces 3); filled = [ "o" ] };
        { index = 2; empty = (empty_spaces 3); filled = [ "x" ] };
        { index = 3; empty = (empty_spaces 4); filled = [] };
        { index = 4; empty = (empty_spaces 4); filled = [] };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 3"
      [
        { index = 1; empty = (empty_spaces 2); filled = [ "o"; "o" ] };
        { index = 2; empty = (empty_spaces 2); filled = [ "x"; "x" ] };
        { index = 3; empty = (empty_spaces 4); filled = [] };
        { index = 4; empty = (empty_spaces 4); filled = [] };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 4"
      [
        { index = 1; empty = (empty_spaces 4); filled = [] };
        { index = 2; empty = (empty_spaces 4); filled = [] };
        { index = 3; empty = (empty_spaces 2); filled = [ "o"; "o" ] };
        { index = 4; empty = (empty_spaces 2); filled = [ "x"; "x" ] };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 5"
      [
        { index = 1; empty = (empty_spaces 3); filled = [ "o" ] };
        { index = 2; empty = (empty_spaces 2); filled = [ "x"; "o" ] };
        { index = 3; empty = (empty_spaces 2); filled = [ "o"; "x" ] };
        { index = 4; empty = (empty_spaces 3); filled = [ "x" ] };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 6"
      [
        { index = 1; empty = (empty_spaces 2); filled = [ "x"; "o" ] };
        { index = 2; empty = (empty_spaces 2); filled = [ "x"; "o" ] };
        { index = 3; empty = (empty_spaces 2); filled = [ "x"; "o" ] };
        { index = 4; empty = (empty_spaces 3); filled = [ "x" ] };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 7"
      [
        { index = 1; empty = (empty_spaces 2); filled = [ "o"; "x" ] };
        { index = 2; empty = (empty_spaces 2); filled = [ "o"; "x" ] };
        { index = 3; empty = (empty_spaces 2); filled = [ "o"; "x" ] };
        { index = 4; empty = (empty_spaces 3); filled = [ "x" ] };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 8"
      [
        { index = 1; empty = (empty_spaces 2); filled = [ "o"; "o" ] };
        { index = 2; empty = (empty_spaces 2); filled = [ "x"; "x" ] };
        { index = 3; empty = (empty_spaces 4); filled = [] };
        { index = 4; empty = (empty_spaces 4); filled = [] };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 9"
      [
        { index = 1; empty = (empty_spaces 4); filled = [] };
        { index = 2; empty = (empty_spaces 4); filled = [] };
        { index = 3; empty = (empty_spaces 2); filled = [ "o"; "o" ] };
        { index = 4; empty = (empty_spaces 2); filled = [ "x"; "x" ] };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 10"
      [
        { index = 1; empty = (empty_spaces 4); filled = [] };
        { index = 2; empty = (empty_spaces 2); filled = [ "x"; "o" ] };
        { index = 3; empty = (empty_spaces 2); filled = [ "o"; "x" ] };
        { index = 4; empty = (empty_spaces 4); filled = [] };
      ]
      (0, 0);
    (* [ Edge Case 2 ] Boards with a large number of columns *)
    score_tuple_test "score_tuple test 11"
      [
        { index = 1; empty = (empty_spaces 24); filled = [] };
        { index = 2; empty = (empty_spaces 24); filled = [] };
        { index = 3; empty = (empty_spaces 24); filled = [] };
        { index = 4; empty = (empty_spaces 24); filled = [] };
        { index = 5; empty = (empty_spaces 24); filled = [] };
        { index = 6; empty = (empty_spaces 24); filled = [] };
        { index = 7; empty = (empty_spaces 24); filled = [] };
        { index = 8; empty = (empty_spaces 24); filled = [] };
        { index = 9; empty = (empty_spaces 24); filled = [] };
        { index = 10; empty = (empty_spaces 24); filled = [] };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 12"
      [
        { index = 1; empty = (empty_spaces 24); filled = [] };
        { index = 2; empty = (empty_spaces 24); filled = [] };
        { index = 3; empty = (empty_spaces 24); filled = [] };
        { index = 4; empty = (empty_spaces 24); filled = [] };
        { index = 5; empty = (empty_spaces 24); filled = [] };
        { index = 6; empty = (empty_spaces 24); filled = [] };
        { index = 7; empty = (empty_spaces 24); filled = [] };
        { index = 8; empty = (empty_spaces 24); filled = [] };
        { index = 9; empty = (empty_spaces 24); filled = [] };
        { index = 10; empty = (empty_spaces 24); filled = [] };
        { index = 11; empty = (empty_spaces 24); filled = [] };
        { index = 12; empty = (empty_spaces 24); filled = [] };
        { index = 13; empty = (empty_spaces 24); filled = [] };
        { index = 14; empty = (empty_spaces 24); filled = [] };
        { index = 15; empty = (empty_spaces 24); filled = [] };
        { index = 16; empty = (empty_spaces 24); filled = [] };
        { index = 17; empty = (empty_spaces 24); filled = [] };
        { index = 18; empty = (empty_spaces 24); filled = [] };
        { index = 19; empty = (empty_spaces 24); filled = [] };
        { index = 20; empty = (empty_spaces 24); filled = [] };
      ]
      (0, 0);
    (* [Edge Case 3 ]Board with a large number of rows *)
    score_tuple_test "score_tuple test 13"
      [
        {
          index = 1;
          empty = (empty_spaces 10);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 10);
          filled = [];
        };
        {
          index = 3;
          empty = (empty_spaces 10);
          filled = [];
        };
        {
          index = 4;
          empty = (empty_spaces 10);
          filled = [];
        };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 14"
      [
        {
          index = 1;
          empty = (empty_spaces 9);
          filled = [ "x" ];
        };
        {
          index = 2;
          empty = (empty_spaces 9);
          filled = [ "o" ];
        };
        {
          index = 3;
          empty = (empty_spaces 10);
          filled = [];
        };
        {
          index = 4;
          empty = (empty_spaces 10);
          filled = [];
        };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 15"
      [
        {
          index = 1;
          empty = (empty_spaces 8);
          filled = [ "x"; "o" ];
        };
        {
          index = 2;
          empty = (empty_spaces 8);
          filled = [ "x"; "o" ];
        };
        {
          index = 3;
          empty = (empty_spaces 8);
          filled = [ "x"; "o" ];
        };
        {
          index = 4;
          empty = (empty_spaces 9);
          filled = [ "x" ];
        };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 16"
      [
        {
          index = 1;
          empty = (empty_spaces 8);
          filled = [ "o"; "x" ];
        };
        {
          index = 2;
          empty = (empty_spaces 8);
          filled = [ "o"; "x" ];
        };
        {
          index = 3;
          empty = (empty_spaces 8);
          filled = [ "o"; "x" ];
        };
        {
          index = 4;
          empty = (empty_spaces 9);
          filled = [ "x" ];
        };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 17"
      [
        {
          index = 1;
          empty =
            (empty_spaces 19);
          filled = [ "x" ];
        };
        {
          index = 2;
          empty =
            (empty_spaces 19);
          filled = [ "o" ];
        };
        {
          index = 3;
          empty =
            (empty_spaces 20);
          filled = [];
        };
        {
          index = 4;
          empty =
            (empty_spaces 20);
          filled = [];
        };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 18"
      [
        {
          index = 1;
          empty =
            (empty_spaces 18);
          filled = [ "x"; "o" ];
        };
        {
          index = 2;
          empty =
            (empty_spaces 18);
          filled = [ "x"; "o" ];
        };
        {
          index = 3;
          empty =
            (empty_spaces 18);
          filled = [ "x"; "o" ];
        };
        {
          index = 4;
          empty =
            (empty_spaces 19);
          filled = [ "x" ];
        };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 19"
      [
        {
          index = 1;
          empty =
            (empty_spaces 18);
          filled = [ "o"; "x" ];
        };
        {
          index = 2;
          empty =
            (empty_spaces 18);
          filled = [ "o"; "x" ];
        };
        {
          index = 3;
          empty =
            (empty_spaces 18);
          filled = [ "o"; "x" ];
        };
        {
          index = 4;
          empty =
            (empty_spaces 19);
          filled = [ "x" ];
        };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 20"
      [
        {
          index = 1;
          empty =
            (empty_spaces 16);
          filled = [ "x"; "x"; "x"; "x" ];
        };
        {
          index = 2;
          empty =
            (empty_spaces 17);
          filled = [ "o"; "o"; "o" ];
        };
        {
          index = 3;
          empty =
            (empty_spaces 20);
          filled = [];
        };
        {
          index = 4;
          empty =
            (empty_spaces 20);
          filled = [];
        };
      ]
      (0, 0);
    (* Typical input [Default board size (6 x 7)] *)
    score_tuple_test "score_tuple test 21"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 3;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 4;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 5;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 6;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 22"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 3;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 4;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 5;
          empty = (empty_spaces 5);
          filled = [ "x" ];
        };
        {
          index = 6;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      (0, 0);
    score_tuple_test "score_tuple test 23"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 3;
          empty = (empty_spaces 2);
          filled = [ "x"; "x"; "x"; "x" ];
        };
        {
          index = 4;
          empty = (empty_spaces 3);
          filled = [ "o"; "o"; "o" ];
        };
        {
          index = 5;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 6;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      (0, 0);
  ]

let is_game_over_pvp_tests =
  [
    is_game_over_pvp_test "is_game_over_pvp test 1"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 3;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 4;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 5;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 6;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      false;
    is_game_over_pvp_test "is_game_over_pvp test 2"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 3;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 4;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 5;
          empty = (empty_spaces 5);
          filled = [ "x" ];
        };
        {
          index = 6;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      false;
    is_game_over_pvp_test "is_game_over_pvp test 3"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 3;
          empty = (empty_spaces 2);
          filled = [ "x"; "x"; "x"; "x" ];
        };
        {
          index = 4;
          empty = (empty_spaces 3);
          filled = [ "o"; "o"; "o" ];
        };
        {
          index = 5;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 6;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      false;
    is_game_over_pvp_test "is_game_over_pvp test 4"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 3);
          filled = [ "x"; "o"; "x" ];
        };
        {
          index = 3;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 4;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 5;
          empty = (empty_spaces 4);
          filled = [ "o"; "o" ];
        };
        {
          index = 6;
          empty = (empty_spaces 5);
          filled = [ "x" ];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      false;
    is_game_over_pvp_test "is_game_over_pvp test 5"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 5);
          filled = [ "x" ];
        };
        {
          index = 3;
          empty = (empty_spaces 2);
          filled = [ "o"; "o"; "o"; "o" ];
        };
        {
          index = 4;
          empty = (empty_spaces 3);
          filled = [ "x"; "x"; "x" ];
        };
        {
          index = 5;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 6;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      false;
  ]

let is_game_over_pvc_tests =
  [
    is_game_over_pvc_test "is_game_over_pvc test 1"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 3;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 4;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 5;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 6;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      false;
    is_game_over_pvc_test "is_game_over_pvc test 2"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 3;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 4;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 5;
          empty = (empty_spaces 5);
          filled = [ "x" ];
        };
        {
          index = 6;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      false;
    is_game_over_pvc_test "is_game_over_pvc test 3"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 3;
          empty = (empty_spaces 2);
          filled = [ "x"; "x"; "x"; "x" ];
        };
        {
          index = 4;
          empty = (empty_spaces 3);
          filled = [ "o"; "o"; "o" ];
        };
        {
          index = 5;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 6;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      false;
    is_game_over_pvc_test "is_game_over_pvc test 4"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 3);
          filled = [ "x"; "o"; "x" ];
        };
        {
          index = 3;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 4;
          empty = (empty_spaces 4);
          filled = [ "o"; "x" ];
        };
        {
          index = 5;
          empty = (empty_spaces 4);
          filled = [ "o"; "o" ];
        };
        {
          index = 6;
          empty = (empty_spaces 5);
          filled = [ "x" ];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      false;
    is_game_over_pvc_test "is_game_over_pvc test 5"
      [
        {
          index = 1;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 2;
          empty = (empty_spaces 5);
          filled = [ "x" ];
        };
        {
          index = 3;
          empty = (empty_spaces 2);
          filled = [ "o"; "o"; "o"; "o" ];
        };
        {
          index = 4;
          empty = (empty_spaces 3);
          filled = [ "x"; "x"; "x" ];
        };
        {
          index = 5;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 6;
          empty = (empty_spaces 6);
          filled = [];
        };
        {
          index = 7;
          empty = (empty_spaces 6);
          filled = [];
        };
      ]
      false;
  ]

let check_tests =
  right_diag_fours_tests @ left_diag_fours_tests
  @ indices_to_strings_tests @ score_tuple_tests
  @ is_game_over_pvp_tests @ is_game_over_pvc_tests

let init_state_tests =
  [
    init_state_test "init_state_test 3x3" 3 3
      {
        turn = true;
        board =
          [
            { index = 1; empty = (empty_spaces 3); filled = [] };
            { index = 2; empty = (empty_spaces 3); filled = [] };
            { index = 3; empty = (empty_spaces 3); filled = [] };
          ];
        move = 0;
      };
    init_state_test "init_state_test 2x2" 2 2
      {
        turn = true;
        board =
          [
            { index = 1; empty = (empty_spaces 2); filled = [] };
            { index = 2; empty = (empty_spaces 2); filled = [] };
          ];
        move = 0;
      };
  ]

let place_token_tests =
  [
    place_token_test "place_token_test 1"
      {
        turn = true;
        board = [ { index = 1; empty = [ "_" ]; filled = [ "O" ] } ];
        move = 0;
      }
      "X" 1 []
      ( {
          turn = false;
          board = [ { index = 1; empty = []; filled = [ "X"; "O" ] } ];
          move = 1;
        },
        [
          {
            turn = true;
            board = [ { index = 1; empty = [ "_" ]; filled = [ "O" ] } ];
            move = 0;
          };
        ] );
    place_token_test "place_token_test 2"
      {
        turn = true;
        board = [ { index = 1; empty = (empty_spaces 2); filled = [] } ];
        move = 0;
      }
      "O" 1 []
      ( {
          turn = false;
          board = [ { index = 1; empty = [ "_" ]; filled = [ "O" ] } ];
          move = 1;
        },
        [
          {
            turn = true;
            board = [ { index = 1; empty = (empty_spaces 2); filled = [] } ];
            move = 0;
          };
        ] );
    place_token_error_test "place_token_error_test 1"
      {
        turn = true;
        board = [ { index = 1; empty = []; filled = [ "X"; "O" ] } ];
        move = 0;
      }
      "X" 1 [] ColumnFull;
    place_token_error_test "place_token_error_test 2"
      {
        turn = true;
        board = [ { index = 1; empty = [ "_" ]; filled = [ "X" ] } ];
        move = 0;
      }
      "O" 3 [] OutOfIndex;
    place_token_error_test "place_token_error_test 3"
      {
        turn = true;
        board = [ { index = 1; empty = [ "_" ]; filled = [ "X" ] } ];
        move = 0;
      }
      "O" 0 [] OutOfIndex;
  ]

let flip_tests = []

let undo_tests =
  [
    undo_test "undo_test empty"
      [
        {
          turn = true;
          board = [ { index = 2; empty = (empty_spaces 2); filled = [] } ];
          move = 0;
        };
      ]
      ( {
          turn = true;
          board = [ { index = 2; empty = (empty_spaces 2); filled = [] } ];
          move = 0;
        },
        [] );
    undo_test "undo_test empty"
      [
        {
          turn = true;
          board =
            [
              { index = 1; empty = (empty_spaces 2); filled = [] };
              { index = 2; empty = [ "X"; "O" ]; filled = [] };
            ];
          move = 1;
        };
        {
          turn = true;
          board =
            [
              { index = 1; empty = (empty_spaces 2); filled = [] };
              { index = 2; empty = (empty_spaces 2); filled = [] };
            ];
          move = 2;
        };
      ]
      ( {
          turn = true;
          board =
            [
              { index = 1; empty = (empty_spaces 2); filled = [] };
              { index = 2; empty = [ "X"; "O" ]; filled = [] };
            ];
          move = 1;
        },
        [
          {
            turn = true;
            board =
              [
                { index = 1; empty = (empty_spaces 2); filled = [] };
                { index = 2; empty = (empty_spaces 2); filled = [] };
              ];
            move = 2;
          };
        ] );
    undo_error_test "undo_error_test empty" [] NoMoveMade;
  ]

let state_tests =
  init_state_tests @ place_token_tests @ flip_tests @ undo_tests

let string_to_command_tests =
  [
    string_to_command_test "string_to_command_test 1" "1" (Insert 1);
    string_to_command_test "string_to_command_test 2" "flip" FlipBoard;
    string_to_command_test "string_to_command_test 3" "undo" Undo;
    string_to_command_test "string_to_command_test 4" "quit" Quit;
    string_to_command_test "string_to_command_test 5" "commands"
      Commands;
    string_to_command_test "string_to_command_test 6" "save test_name"
      (Save [ "test_name" ]);
    string_to_command_error_test "string_to_command_error_test 1"
      "invalid command" InvalidCommand;
  ]

let save_tests =
  [
    save_file_test "save_file_test 1"
      {
        turn = true;
        board = [ { index = 1; empty = [ "_" ]; filled = [ "O" ] } ];
        move = 0;
      }
      [] [ "regular_filename" ] true;
    save_file_error_test "save_file_error_test empty"
      {
        turn = true;
        board = [ { index = 1; empty = [ "_" ]; filled = [ "O" ] } ];
        move = 0;
      }
      [] [] InvalidFilename;
    save_file_error_test "save_file_error_test 2 words"
      {
        turn = true;
        board = [ { index = 1; empty = [ "_" ]; filled = [ "O" ] } ];
        move = 0;
      }
      [] [ "qwdw"; "qwd" ] InvalidFilename;
    save_file_error_test "save_file_error_test empty string"
      {
        turn = true;
        board = [ { index = 1; empty = [ "_" ]; filled = [ "O" ] } ];
        move = 0;
      }
      [] [ "" ] InvalidFilename;
    save_file_error_test "save_file_error_test quit"
      {
        turn = true;
        board = [ { index = 1; empty = [ "_" ]; filled = [ "O" ] } ];
        move = 0;
      }
      [] [ "quit" ] InvalidFilename;
  ]

let open_tests =
  [
    open_file_test "open_file_test 1"
      ("game_files" ^ Filename.dir_sep ^ "testfile.json")
      ( {
          turn = false;
          board =
            [
              {
                index = 1;
                empty = (empty_spaces 6);
                filled = [];
              };
              {
                index = 2;
                empty = (empty_spaces 5);
                filled = [ "X" ];
              };
              {
                index = 3;
                empty = (empty_spaces 6);
                filled = [];
              };
              {
                index = 4;
                empty = (empty_spaces 6);
                filled = [];
              };
              {
                index = 5;
                empty = (empty_spaces 6);
                filled = [];
              };
              {
                index = 6;
                empty = (empty_spaces 6);
                filled = [];
              };
              {
                index = 7;
                empty = (empty_spaces 6);
                filled = [];
              };
            ];
          move = 1;
        },
        [
          {
            turn = true;
            board =
              [
                {
                  index = 1;
                  empty = (empty_spaces 6);
                  filled = [];
                };
                {
                  index = 2;
                  empty = (empty_spaces 6);
                  filled = [];
                };
                {
                  index = 3;
                  empty = (empty_spaces 6);
                  filled = [];
                };
                {
                  index = 4;
                  empty = (empty_spaces 6);
                  filled = [];
                };
                {
                  index = 5;
                  empty = (empty_spaces 6);
                  filled = [];
                };
                {
                  index = 6;
                  empty = (empty_spaces 6);
                  filled = [];
                };
                {
                  index = 7;
                  empty = (empty_spaces 6);
                  filled = [];
                };
              ];
            move = 0;
          };
        ] );
    open_file_error_test "open_file_error_test empty" "not_a_file"
      InvalidFilename;
  ]

let menu_tests = []
let command_tests = string_to_command_tests

let tests =
  board_tests @ check_tests @ state_tests @ command_tests @ save_tests
  @ open_tests

(*****************************************************************)
(* Test suite *)
(*****************************************************************)

let suite = "search test suite" >::: List.flatten [ tests ]
let _ = run_test_tt_main suite
