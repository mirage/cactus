type t = string

let empty = ""

let sub = String.sub

let length = String.length

let concat = String.concat ""

let padder (c : char) n s =
  let m = String.length s in
  if m > n then failwith "Cannot pad" else String.make (n - m) c ^ s

let unpad c_pad s =
  let _, padd =
    Seq.fold_left
      (fun (found, off) c ->
        match found with
        | true -> (found, off)
        | false -> if c = c_pad then (false, off + 1) else (true, off))
      (false, 0) (String.to_seq s)
  in
  String.sub s padd (String.length s - padd)

let to_b256 n =
  let rec aux acc n =
    match n with
    | 0 -> acc |> List.to_seq |> String.of_seq
    | _ ->
        let q, r = (n mod 256, n lsr 8) in
        aux (Char.chr q :: acc) r
  in
  aux [] n

let from_b256 s =
  s
  |> String.to_seq
  |> List.of_seq
  |> List.map Char.code
  |> List.fold_left (fun n digit -> (n lsl 8) + digit) 0

let encode_int ~pad i = i |> to_b256 |> padder '\000' pad

let decode_int = from_b256

let encode_string ?pad s = match pad with None -> s | Some pad -> padder '\001' pad s

let decode_string = unpad '\001'

let encode_bool ~pad b = String.make pad (if b then '\255' else '\000')

let decode_bool s =
  match s.[0] |> Char.code with
  | 0 -> false
  | 255 -> true
  | c -> Format.kasprintf failwith "Unknown bool encoding with char %i" c

let chunkate_list n l =
  let rec aux count current acc l =
    match l with
    | [] -> current :: acc
    | h :: t ->
        if count = 0 then aux (n - 1) [ h ] (current :: acc) t
        else aux (count - 1) (h :: current) acc t
  in
  aux n [] [] l |> List.rev |> List.map List.rev

let rec strip n l = match n with 0 -> l | _ -> strip (n - 1) (List.tl l)

let encode_bools n bools =
  assert (List.length bools = n);
  let bool_to_bit b = if b then 1 else 0 in
  let bits_to_int bits = List.fold_left (fun s bit -> (2 * s) + bit) 0 bits in
  bools
  |> List.map bool_to_bit
  |> chunkate_list 8
  |> List.map (fun bs -> bs |> bits_to_int |> Char.chr)
  |> List.to_seq
  |> String.of_seq

let decode_bools n s =
  let bit_to_bool b = match b with 0 -> false | 1 -> true | _ -> failwith "Not a bit" in
  let byte_to_bits n =
    let rec aux count acc n =
      if count = 0 then acc else aux (count - 1) ((n mod 2) :: acc) (n lsr 1)
    in
    aux 8 [] n
  in
  let chunks =
    s |> String.to_seq |> List.of_seq |> List.map (fun c -> c |> Char.code |> byte_to_bits)
  in
  (* strip the padding, which lives in the last chunk *)
  let last_chunk = chunks |> List.rev |> List.hd |> strip ((8 - (n mod 8)) mod 8) in
  chunks
  |> List.rev
  |> List.tl
  |> List.cons last_chunk
  |> List.rev
  |> List.flatten
  |> List.map bit_to_bool

let dump s = s

let load s = s

open Fmt

let pp ppf s = s |> Hex.of_string |> Hex.show |> pf ppf "%s"
