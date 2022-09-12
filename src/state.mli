type t = {
  turn : bool;
  board : Board.t;
  move : int;
}
(** The abstract type of values representing the game state *)

val get_turn : t -> bool
val get_board : t -> Board.t
val get_move : t -> int

val init_state : int -> int -> t
(** [init_state row col] is the initial state of the game with an empty
    board with row rows and col columns *)

(* val eval : Board.t -> int * int *)
(** [eval board] score for both players *)

exception ColumnFull
(** Raised when a full column is chosen to place the token *)

exception OutOfIndex
(** Raised when the column chosen is not one of the choices *)

exception NoMoveMade
(** Raised when trying to undo an empty board *)

val place_token : t -> string -> int -> t list -> t * t list
(** [place_token state token index] returns a pair with an updated state
    where a token has been played in column number index on the board,
    the move number has incremented, and the turn has changed, and a
    history list with the [state] added to the front. *)

val flip : t -> t list -> t * t list
(** [flip state] returns a pair with an updated state where the board
    has the tokens upside down, the move number has incremented, and the
    turn has changed, and a history list with the [state] added to the
    front. *)

val undo : t list -> t * t list
(** [undo history] takes in history, a list of states, and returns a
    pair with first element of history and a list of the rest of history *)
