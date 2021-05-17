module type K = sig
  type t [@@deriving repr]
  (** The type for keys. *)

  type input_key

  val size : int

  val min : t (* min key *)

  val max : t (* max key *)

  val of_input : input_key -> t

  val to_input : t -> input_key

  val empty : t

  val equal : t -> t -> bool
  (** The equality function for keys. *)

  val compare : t -> t -> int

  val length : t -> int

  val common_prefix : t -> t -> t

  val sub : t -> off:int -> len:int -> t

  val concat : t -> t -> t

  val encode : t -> Encoder.t
  (** [encode] is an encoding function. *)

  val decode : Encoder.t -> t
  (** Must satisfy [decode (encode t) = t]. *)

  val debug_dump : t -> string

  val pp : Format.formatter -> t -> unit

  val pp_encoded : Format.formatter -> Encoder.t -> unit
end

module type V = sig
  type t [@@deriving repr]
  (** The type for values. *)

  type input_value

  val size : int

  val of_input : input_value -> t

  val to_input : t -> input_value

  val encode : t -> Encoder.t
  (** [encode] is an encoding function. *)

  val decode : Encoder.t -> t
  (** Must satisfy [decode (encode t) = t]. *)

  val pp : Format.formatter -> t -> unit

  val pp_encoded : Format.formatter -> Encoder.t -> unit
end

module type Entry = sig
  type input_key

  type input_value

  module Key : K with type input_key = input_key

  module Value : V with type input_value = input_value
end

module Make : functor (InKey : Input.Key) (InValue : Input.Value) ->
  Entry with type input_key = InKey.t and type input_value = InValue.t =
functor
  (InKey : Input.Key)
  (InValue : Input.Value)
  ->
  struct
    type input_key = InKey.t

    type input_value = InValue.t

    module Key = struct
      type t = string [@@deriving repr]

      type input_key = InKey.t

      let size = InKey.encoded_size

      let min = Utils.min_key size

      let max = Utils.max_key size

      let empty = ""

      let equal = String.equal

      let compare = String.compare

      let length = String.length

      let common_prefix k1 k2 =
        String.of_seq
          Seq.(
            unfold
              (fun (seq1, seq2) ->
                match (seq1 (), seq2 ()) with
                | Nil, _ | _, Nil -> None
                | Cons (c1, seq1'), Cons (c2, seq2') when c1 = c2 -> Some (c1, (seq1', seq2'))
                | _ -> None)
              (String.to_seq k1, String.to_seq k2))

      let encode = Encoder.load

      let decode = Encoder.dump

      let concat k1 k2 = k1 ^ k2

      let sub t ~off ~len = String.sub t off len

      let of_input k =
        let ret = InKey.encode k in
        assert (size = String.length ret);
        ret

      let to_input = InKey.decode

      let debug_dump s = s

      open Fmt

      let ascii =
        String.map (fun c ->
            let code = Char.code c in
            if 0x20 <= code && code <= 0x7E then c else '.')

      let pp ppf s = pf ppf "%a" (string |> styled (`Bg `Green) |> styled `Reverse) (ascii s)

      let pp_encoded ppf encoded =
        pf ppf "%a%a" (Encoder.pp |> styled (`Bg `Green)) encoded pp (encoded |> decode)
    end

    module Value = struct
      type t = string [@@deriving repr]

      type input_value = InValue.t

      let size = InValue.encoded_size

      let encode s = Encoder.encode_string s

      let decode = Encoder.decode_string

      let of_input v =
        let ret = InValue.encode v in
        assert (size = String.length ret);
        ret

      let to_input = InValue.decode

      open Fmt

      let pp = string |> styled (`Bg `Blue) |> styled `Reverse

      let pp_encoded ppf encoded =
        pf ppf "%a%a" (Encoder.pp |> styled (`Bg `Blue)) encoded pp (encoded |> decode)
    end
  end
