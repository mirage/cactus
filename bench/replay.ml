open Encoding

module Key : Btree.Input.Key with type t = Key.t = struct
  include Key

  let decode s = decode s 0
end

module Value : Btree.Input.Value with type t = Val.t = struct
  include Val

  let decode s = decode s 0
end

module Btree = Btree.Make (Key) (Value) (Btree.Input.Default.Size)

let ( // ) a b = a ^ "/" ^ b

let init () =
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
  Printexc.record_backtrace true;
  Memtrace.trace_if_requested ();
  Utils.clean "_bench/replay/";
  Logs.set_reporter Log.app_reporter

let main trace _ =
  init ();
  match trace with
  | None ->
      Fmt.pr "%a A trace file must be specified@." Fmt.(styled (`Fg `Red) string) "[Parse error]";
      Cmdliner.Term.exit (`Error `Parse)
  | Some trace ->
      let root = "_bench/replay" in
      let tree = Btree.create root in
      Btree.replay ~prog:`Single trace tree

open Cmdliner

let env_var s = Arg.env_var ("BTREE_REPLAY_" ^ s)

let trace =
  let doc = "Path to a btree.trace" in
  let env = env_var "TRACE" in
  Arg.(value & pos 0 (some non_dir_file) None & info [] ~doc ~env)

let setup_log = Term.(const Log.setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd =
  let doc = "Replay a trace" in
  (Term.(const main $ trace $ setup_log), Term.info "replay" ~doc ~exits:Term.default_exits)

let () = Term.exit @@ Term.eval cmd
