type column = {
  index : int;
  empty : string list;
  filled : string list;
}
(** The type [column] represents a single column in the board. It
    contains information about which column it is, what tokens are in
    it, and how many empty spaces it contains *)

type t = column list
(** The type [t] represents the board as a list of columns *)

(* val cols_to_matrix : column list -> string list list *)

val get_index : column -> int
val get_empty : column -> string list
val get_filled : column -> string list
val get_board_as_matrix : t -> string list list
val make_column : int -> string list -> string list -> column
val transpose : string list list -> string list list
val make_board : int -> int -> t

val print_board : t -> unit
(** [print_board board] prints out a graphical representation of the
    board *)

val fill_empty : int -> string list
(** [fill_empty n] returns a list with n underscores ("_") *)

val cols_to_matrix : column list -> string list list
