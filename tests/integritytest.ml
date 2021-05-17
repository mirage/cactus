include Common

let ( // ) a b = a ^ "/" ^ b

let test_creation version () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // "integrity_creation" in
  for _ = 1 to 10 do
    MyBtree.create ~root |> ignore
  done

let test_addition version n () =
  let module MyBtree = (val get_tree version) in
  let increment = n / 10 in
  let root = v_to_s version // Format.sprintf "integrity_addition_%i" n in
  let keys = Array.init (n + 1) (fun _ -> generate_key ()) in
  for i = 1 to 10 do
    let tree = MyBtree.create ~root in
    MyBtree.snapshot tree;
    for _j = 1 to i * increment do
      MyBtree.add tree keys.(i) (0, 0, 0)
    done
  done

let test_mem version n () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // "integrity_mem" in
  let increment = n / 10 in
  let keys = Array.init (n + 1) (fun _ -> generate_key ()) in
  for i = 1 to 10 do
    let tree = MyBtree.create ~root in
    for j = 1 to i * increment do
      MyBtree.add tree keys.(j) (0, 0, 0)
    done;
    MyBtree.flush tree;
    let tree = MyBtree.create ~root in
    for j = 1 to i * increment do
      Alcotest.(check bool)
        (Format.sprintf "Checking that key %s is indeed there" keys.(j))
        true
        (MyBtree.mem tree keys.(j))
    done
  done

let suite version =
  ( Fmt.str "%s integrity" (v_to_s version),
    [
      ("Creation", `Quick, test_creation version);
      ("Addition", `Quick, test_addition version 1000);
      ("Big addition", `Quick, test_addition version 10000);
      ("Mem", `Quick, test_mem version 100);
    ] )
