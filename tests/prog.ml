module V = (val Oracle.stringv ~encode_sz:15)

module H = Oracle.Default

module Sort = Oracle.Make (V) (H)

let () = Printexc.record_backtrace true

let () = Random.init 42

let random_char _seed = char_of_int (33 + Random.int 94)

let oracle () = String.init (V.encode_sz - 1) random_char ^ "\n"

let run n =
  if not (Sys.file_exists "_tests") then Unix.mkdir "_tests" 0o777;
  let root = "_tests/progress" in
  if not (Sys.file_exists root) then Unix.mkdir root 0o777;
  let out = root ^ "/" ^ "out" in
  Sort.sort ~with_prog:true ~oracle ~out n

let () = run 1_000_000
