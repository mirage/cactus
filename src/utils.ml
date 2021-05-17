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

let rec last_elem l =
  match l with [] -> failwith "Empty list" | [ e ] -> e | _ :: t -> last_elem t

(** range function *)
let rec ( -- ) a b = if a > b then [] else a :: (a + 1 -- b)

let rec iter func n =
  if n > 0 then (
    func ();
    iter func (n - 1))

let replace s offset replacement =
  let length = Encoder.length s in
  let replacement_length = min (Encoder.length replacement) (length - offset) in
  Encoder.concat
    [
      Encoder.sub s 0 offset;
      Encoder.sub replacement 0 replacement_length;
      Encoder.sub s (offset + replacement_length) (length - offset - replacement_length);
    ]

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

let rec map3 func l1 l2 l3 =
  (* the inexistent List.map3 *)
  match (l1, l2, l3) with
  | [], [], [] -> []
  | a1 :: l1, a2 :: l2, a3 :: l3 -> func a1 a2 a3 :: map3 func l1 l2 l3
  | _ -> failwith "Invalid argument"

let chunkate ?(safe = true) ~off n s =
  (* split [s] in chunks of size [n], except possibly for the last chunk, which is dropped if [safe] is [false]*)
  let l = Encoder.length s in
  let rec aux acc i =
    if l - i = n then Encoder.sub s i (l - i) :: acc |> List.rev
    else if l - i < n then if safe then Encoder.sub s i (l - i) :: acc |> List.rev else acc
    else aux (Encoder.sub s i n :: acc) (i + n)
  in
  if off = 0 then aux [] 0 else Encoder.sub s 0 off :: aux [] off

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

let eat_offset offset sizes =
  let rec aux total sizes =
    match sizes with
    | [] -> []
    | size :: sizes ->
        if total + size >= offset then (total + size - offset) :: sizes
        else aux (total + size) sizes
  in
  aux 0 sizes

let chunkate_with ~off ns s =
  let l = Encoder.length s in
  let ns = eat_offset off ns in
  let rec aux acc i ns =
    match ns with
    | [] -> acc |> List.rev
    | n :: ns ->
        let n = min n (l - i) in
        aux (Encoder.sub s i n :: acc) (i + n) ns
  in
  aux [] 0 ns

let rec pop_n n liste =
  match (n, liste) with
  | 0, _ -> liste
  | _, [] -> failwith "Too much to pop"
  | _, _ :: t -> pop_n (n - 1) t

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
