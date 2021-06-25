include Common

let ( // ) a b = a ^ "/" ^ b

let test_replay version () =
  let module MyBtree = (val get_tree version) in
  let root1 = v_to_s version // "replay_origin" in
  let trace = root1 // "btree.trace" in
  let tree1 = MyBtree.create ~record:trace root1 in
  let keys = Array.init 101 (fun _ -> generate_key ()) in
  Array.iteri (fun i key -> MyBtree.add tree1 key (i, i, i)) keys;
  MyBtree.close tree1;
  let root2 = v_to_s version // "replay_dist" in
  let tree2 = MyBtree.create root2 in
  MyBtree.replay trace tree2;
  MyBtree.close tree2;
  let d1 = Digest.file (root1 // "b.tree") in
  let d2 = Digest.file (root2 // "b.tree") in
  Alcotest.(check string) "Equality between origin and replay" d1 d2

let suite version =
  (Fmt.str "%s replay" (v_to_s version), [ ("Replay", `Quick, test_replay version) ])
