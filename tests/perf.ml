module V = (val Oracle.stringv ~encode_sz:15)

module H = Oracle.Default

module Sort = Oracle.Make (V) (H)

let () = Printexc.record_backtrace true

let () = Random.init 42

let random_char _seed = char_of_int (33 + Random.int 94)

let oracle () = String.init (V.encode_sz - 1) random_char ^ "\n"

let rec power base expo =
  match expo with
  | 0 -> 1
  | 1 -> base
  | _ ->
      let sqrt = power base (expo / 2) in
      if expo mod 2 = 0 then sqrt * sqrt else sqrt * sqrt * base

let () =
  if not (Sys.file_exists "_tests") then Unix.mkdir "_tests" 0o777;
  let root = "_tests/perf" in
  if not (Sys.file_exists root) then Unix.mkdir root 0o777;
  let out = root ^ "/" ^ "out" in
  let ns = List.init 28 (fun i -> power 2 i) in
  let times = List.map 
    (fun n -> let c = Mtime_clock.counter () in Sort.sort ~with_prog:true ~oracle ~out n ; Unix.unlink out ; Mtime_clock.count c |> Mtime.Span.to_us) 
    ns in
  Fmt.pr "%a@." Fmt.(list ~sep:(fun ppf () -> Fmt.pf ppf ",@ ") float) times
