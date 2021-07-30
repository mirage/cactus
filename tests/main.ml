let () = Printexc.record_backtrace true

let make_suites version =
  [
    Migration.suite version;
    Treetest.suite version;
    Integritytest.suite version;
    Replaytest.suite version;
  ]

let () = Alcotest.run "Btree storage" ([ Utilstest.suite; Oracletest.suite ] @ make_suites `V0)

(* let () = Demo.snapshot () *)
