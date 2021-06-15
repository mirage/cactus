open Stats.Func
open Stats.Utils

let rec binary_search ?(safe = false) ~compare i j =
  (* Finds [k] such that [compare k >= 0] and [compare (k+1) < 0], or [0] if no such positive [k] exists *)
  (* if [safe] is false, fails if there is no [k] such that [compare k = 0] *)
  if j = i then raise Not_found;
  if j - i < 2 then if safe || compare i = 0 then i else raise Not_found
  else
    let k = i + ((j - i) / 2) in
    if compare k < 0 then binary_search ~compare ~safe i k else binary_search ~safe ~compare k j

let binary_search ?(safe = false) ~compare i j =
  tic stat_binary_search;
  let ret = binary_search ~safe ~compare i j in
  tac stat_binary_search;
  ret

let is_sorted l =
  let rec aux last = function [] -> true | h :: t -> if last > h then false else aux h t in
  match l with
  | [] -> true
  | h :: t ->
      let ret = aux h t in
      if not ret then
        Fmt.pr "%a@." Fmt.(list string) (List.map (fun s -> s |> Hex.of_string |> Hex.show) l);
      ret

let sizes_to_offsets sizes =
  let rec aux acc remaining =
    match (remaining, acc) with
    | [], _ | _, [] -> failwith "sizes_to_offsets"
    | [ _size ], _ -> List.rev acc
    | size :: sizes, off :: _ -> aux ((size + off) :: acc) sizes
  in
  aux [ 0 ] sizes

let min_key length =
  (* creates the smallest key of length [length] *)
  String.make length '\000'

let max_key length =
  (* creates the largest key of length [length] *)
  String.make length '\255'

let really_read fd buff off length =
  (* repeat read until all bytes are read *)
  let rec aux buffer_offset len =
    let r = Unix.read fd buff buffer_offset len in
    if r = 0 || r = len then buffer_offset + r - off (* end of file or everything is read *)
    else (aux [@tailcall]) (buffer_offset + r) (len - r)
  in
  aux off length

let really_write fd buff off length =
  (* repeat write until all bytes are written *)
  let rec aux buffer_offset len =
    let r = Unix.write fd buff buffer_offset len in
    if r = 0 || r = len then buffer_offset + r - off (* end of file or everything is written *)
    else (aux [@tailcall]) (buffer_offset + r) (len - r)
  in
  aux off length

let assert_read fd buff off length =
  let read_sz = really_read fd buff off length in
  if read_sz <> length then (
    Log.err @@ fun reporter ->
    reporter "Tried reading %i but could only read %i" length read_sz;
    assert false)

let assert_write fd buff off length =
  let write_sz = really_write fd buff off length in
  if write_sz <> length then (
    Log.err @@ fun reporter ->
    reporter "Tried writing %i but could only write %i" length write_sz;
    assert false)

let b256size n =
  let rec aux acc n = match n with 0 -> acc | _ -> aux (1 + acc) (n / 256) in
  aux 0 n

let to_b256 n =
  let buff = Bytes.create 8 in
  let rec aux l n =
    match n with
    | 0 -> Bytes.sub_string buff (8 - l) l
    | _ ->
        let q, r = (n mod 256, n / 256) in
        Bytes.set buff (7 - l) (Char.chr q);
        aux (l + 1) r
  in
  aux 0 n

let from_b256 s =
  let rep = ref 0 in
  String.iter (fun c -> rep := Char.code c + (!rep * 256)) s;
  !rep
