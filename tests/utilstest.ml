module Utils = Btree.Private.Utils

let test_binary () =
  for tot = 1 to 10 do
    for i = 0 to tot - 1 do
      let compare k = compare i k in
      Alcotest.(check int)
        (Format.sprintf "Binary search for %i in list of length %i" i tot)
        i
        (Utils.binary_search 0 tot ~compare)
    done
  done

let test_offsets () =
  let sizes = [ 3; 1; 6; 4; 9; 11 ] in
  let offsets = [ 0; 3; 4; 10; 14; 23 ] in
  Alcotest.(check (list int)) "Offsets from sizes" offsets (Utils.sizes_to_offsets sizes)

let test_sorted () =
  let is_sorted = [ "0"; "1"; "2"; "3" ] in
  let is_not_sorted = [ "0"; "1"; "2"; "1" ] in
  Alcotest.(check bool) "Check that the list is indeed sorted" true (Utils.is_sorted is_sorted);
  Alcotest.(check bool)
    "Check that the list is indeed not sorted" false (Utils.is_sorted is_not_sorted)

let suite =
  ( "Utils",
    [
      ("Binary search", `Quick, test_binary);
      ("Sizes to offsets", `Quick, test_offsets);
      ("Is sorted", `Quick, test_sorted);
    ] )
