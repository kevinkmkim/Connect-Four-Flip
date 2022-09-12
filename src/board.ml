type column = {
  index : int;
  empty : string list;
  filled : string list;
}

type t = column list

let get_index column = column.index
let get_empty column = column.empty
let get_filled column = column.filled
let make_column index empty filled = { index; empty; filled }

let rec transpose board =
  match board with
  | [] -> []
  | [] :: x -> transpose x
  | (piece :: row) :: columns ->
      ([ piece ] @ List.map List.hd columns)
      :: transpose (row :: List.map List.tl columns)

let rec fill_empty length =
  match length with
  | 0 -> []
  | x -> "_" :: fill_empty (x - 1)

let rec make_cols row_num col_num =
  match col_num with
  | 0 -> []
  | x ->
      { index = x; empty = fill_empty row_num; filled = [] }
      :: make_cols row_num (x - 1)

let make_board row col = List.rev (make_cols row col)
let cols_to_matrix = List.map (fun x -> x.empty @ x.filled)
let print_row row = print_endline ("|" ^ String.concat "|" row ^ "|")

let rec print_matrix = function
  | [] -> print_string ""
  | h :: t ->
      let () = print_row h in
      print_matrix t

let rec print_index_row = function
  | [] -> print_endline "\n"
  | h :: t ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        (" " ^ string_of_int h.index);
      print_index_row t

let get_board_as_matrix board = board |> cols_to_matrix |> transpose

let print_board board =
  let () = board |> get_board_as_matrix |> print_matrix in
  print_index_row board
