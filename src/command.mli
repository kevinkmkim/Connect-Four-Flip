type t =
  | Insert of int
  | FlipBoard
  | Quit
  | Commands
  | Undo
  | Save of string list
      (** The type [command] represents a player's command to either
          make a move or quit the game *)

exception InvalidCommand
(** Raised when an invalid command is parsed. *)

val string_to_command : string -> t
(** [string_to_command str] parses a player's input into a [command] *)