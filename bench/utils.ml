let mkdir dir = if not (Sys.file_exists dir) then Unix.mkdir dir 0o777

let chdir dir =
  mkdir dir;
  let open Unix in
  chdir dir;
  Format.printf "moving into directory %s\n%!" (Unix.getcwd ())

let ( // ) a b = a ^ "/" ^ b

let clean dir =
  let files = [ "b.tree"; "b.png"; "b.log"; "stats.log" ] in
  List.iter
    (fun file ->
      let file = dir // file in
      if Sys.file_exists file then Unix.unlink file)
    files
