(* Some tests to demonstrate the use of the debug pretty-printing functions *)

include Common

let snapshot () =
  let module MyBtree = (val get_tree `V0) in
  let tree = MyBtree.create ~root:"demo" in
  let keys = Array.init 100 (fun _ -> generate_key ()) in
  for i = 1 to 99 do
    let key = keys.(i) in
    (* dummy key *)
    let value = (i, i + 1, i - 1) in
    (* dummy value*)
    MyBtree.add tree key value
  done;

  (* create one x.ansi file for each node and leaf in [tree] with human readable content. *)
  MyBtree.snapshot tree;

  (* it can be bothersome to find the root and reconstruct the correct path, [go_to_leaf] does it for you *)
  let key_to_analyse = generate_key () in
  let path_to_leaves = MyBtree.Private.go_to_leaf tree key_to_analyse in
  Format.fprintf Format.std_formatter "@[<v 2>Mem path:@,Key %s@ -> %a@ -> %b@]"
    (key_to_analyse |> Repr.to_string MyKey.t)
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ -> ")
       (fun ppf -> Format.fprintf ppf "Page %i"))
    (path_to_leaves |> List.rev) (MyBtree.mem tree key_to_analyse)
