(*
 * Copyright (c) 2021 Tarides <contact@tarides.com>
 * Copyright (c) 2021 Gabriel Belouze <gabriel.belouze@ens.psl.eu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Encoding
module Stats = Btree.Index_stats

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

let get_maxrss () =
  let usage = Rusage.(get Self) in
  let ( / ) = Int64.div in
  Int64.to_int (usage.maxrss / 1024L / 1024L)

let with_timer f =
  let started = Mtime_clock.counter () in
  let a = f () in
  let duration = Mtime_clock.count started in
  (a, duration)

let with_stats_and_timer f =
  Stats.reset_stats ();
  let _, duration = with_timer f in
  let stats = Stats.get () in
  (duration, stats)

let pp_results ~with_stats duration (stats : Stats.t) =
  let gc_mem = Gc.stat () |> fun stat -> stat.top_heap_words * Sys.word_size / 8 / 1_000_000 in
  let maxrss = get_maxrss () in
  Logs.info (fun reporter ->
      reporter "@[<v 0>Total time: %a@,Max memory usage : %i @,Maxrss : %d @, @]" Mtime.Span.pp
        duration gc_mem maxrss);
  if with_stats then
    Logs.info (fun reporter ->
        reporter
          "@[<v 0>Bytes read: %d@,\
           Number of reads: %d@,\
           Bytes written: %d@,\
           Number of writes: %d@,\
           Bytes total: %d@,\
           Number of IO: %d@,\
           @]"
          stats.bytes_read stats.nb_reads stats.bytes_written stats.nb_writes
          (stats.bytes_read + stats.bytes_written)
          (stats.nb_reads + stats.nb_writes))

let main trace with_stats () =
  init ();
  match trace with
  | None ->
      Fmt.pr "%a A trace file must be specified@." Fmt.(styled (`Fg `Red) string) "[Parse error]";
      Cmdliner.Term.exit (`Error `Parse)
  | Some trace ->
      let root = "_bench/replay" in
      let replay () =
        let tree = Btree.create root in
        Btree.replay ~prog:`Multiple trace tree
      in
      let duration, stats = with_stats_and_timer replay in
      pp_results ~with_stats duration stats

open Cmdliner

let env_var s = Arg.env_var ("BTREE_REPLAY_" ^ s)

let trace =
  let doc = "Path to a btree.trace" in
  let env = env_var "TRACE" in
  Arg.(value & pos 0 (some non_dir_file) None & info [] ~doc ~env)

let setup_log = Term.(const Log.setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let with_stats =
  let doc = "Print IO stats." in
  Arg.(value & flag & info [ "stats" ] ~doc)

let cmd =
  let doc = "Replay a trace" in
  ( Term.(const main $ trace $ with_stats $ setup_log),
    Term.info "replay" ~doc ~exits:Term.default_exits )

let () = Term.exit @@ Term.eval cmd
