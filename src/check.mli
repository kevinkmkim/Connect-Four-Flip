open Board

val ( -- ) : int -> int -> int list
val right_diag_fours : int -> int list
val left_diag_fours : int -> int list
val indices_to_strings : string list -> int list -> string list
val score_tuple : column list -> int * int
val is_game_over_pvp : column list -> bool
val is_game_over_pvc : column list -> bool