module Encoder = Btree.Private.Encoder

let test_int () =
  for _ = 1 to 100 do
    let i = Random.int 10000 in
    let pad = 256 in
    Alcotest.(check int)
      (Format.sprintf "Checking that encode_int + decode_int = id")
      i
      (i |> Encoder.encode_int ~pad |> Encoder.decode_int);
    Alcotest.(check int)
      (Format.sprintf "Checking that [encode_int ~pad] has length [pad]")
      pad
      (i |> Encoder.encode_int ~pad |> Encoder.length)
  done

let test_string () =
  for _ = 1 to 100 do
    let s = String.init (Random.int 50) (fun _i -> 2 + Random.int 254 |> Char.chr) in
    (* random string with no '\000' *)
    let pad = 50 in
    Alcotest.(check string)
      (Format.sprintf "Checking that encode_string + decode_string = id")
      s
      (s |> Encoder.encode_string ~pad |> Encoder.decode_string);
    Alcotest.(check int)
      (Format.sprintf "Checking that [encode_string ~pad] has length [pad]")
      pad
      (s |> Encoder.encode_string ~pad |> Encoder.length)
  done

let test_bool () =
  for _ = 1 to 100 do
    let b = Random.int 2 = 1 in
    let pad = 10 in
    Alcotest.(check bool)
      (Format.sprintf "Checking that encode_bool + decode_bool = id")
      b
      (b |> Encoder.encode_bool ~pad |> Encoder.decode_bool);
    Alcotest.(check int)
      (Format.sprintf "Checking that [encode_bool ~pad] has length [pad]")
      pad
      (b |> Encoder.encode_bool ~pad |> Encoder.length)
  done

let suite =
  ( "Encoder",
    [
      ("Encode/Decode int", `Quick, test_int);
      ("Encode/Decode string", `Quick, test_string);
      ("Encode/Decode bool", `Quick, test_bool);
    ] )
