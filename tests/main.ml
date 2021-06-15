let () = Printexc.record_backtrace true

let make_suites version =
  [ Migration.suite version; Treetest.suite version; Integritytest.suite version ]

let () = Alcotest.run "Btree storage" ([ Utilstest.suite ] @ make_suites `V0)

(* let () = Demo.snapshot () *)
