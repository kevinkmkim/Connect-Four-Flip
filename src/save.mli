val save_file : State.t -> State.t list -> string list -> bool
(** [save_file state history name] saves the current state and past
    histories in file [name].json from folder game_files. If it does not
    exist, the file is created instead. The function returns true if it
    works, otherwise false is returned *)

val open_file : string -> State.t * State.t list

exception InvalidFilename
(** Raised when trying to save with an unacceptable file name *)