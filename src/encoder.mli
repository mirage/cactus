type t
(** must behave a lot like string or bytes, with linearity of padding, subbing, etc *)

val empty : t
(** the t word of length 0*)

val sub : t -> int -> int -> t
(** mirrors [String.sub]. This function must be consistent with the padding of the other function. *)

val length : t -> int
(** mirrors [String.length] *)

val concat : t list -> t

val to_b256 : int -> string

val from_b256 : string -> int

val encode_int : pad:int -> int -> t
(** [encode_int] must pad to the left *)

val decode_int : t -> int

val encode_string : ?pad:int -> string -> t
(** [encode_string s] encodes the string [s]. The character ['\000'] and [s] should not start with
    it. [encode_string] must pad to the left. If [pad] is not used then [encode_string] must be
    compatible with concatenations, i.e. [f (x^y)] = [(f x) ^ (f y)] and [f^-1 (f (x^y)) = x^y];
    This is equivalent to [encode_string] when [~pad] is chosen to be the length of the string to
    encode *)

val decode_string : t -> string

val encode_bool : pad:int -> bool -> t
(** [encode_bool] must pad to the left *)

val decode_bool : t -> bool

val encode_bools : int -> bool list -> t

val decode_bools : int -> t -> bool list

val dump : t -> string
(** [dump e] provides a disk writable representation of [e]. Must be consistent with [length]. This
    should only be used by low level IO modules. *)

val load : string -> t
(** the inverse function of [dump]. This should only be use by low level IO modules. *)

val pp : t Fmt.t
