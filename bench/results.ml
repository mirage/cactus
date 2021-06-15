module Stats = Btree.Private.Stats

let with_stat f =
  Stats.reset ();
  let t0 = Sys.time () in
  let _ = f () in
  let t1 = Sys.time () -. t0 in
  let stats = Stats.get () in
  Stats.reset ();
  (t1, stats)

type perf = {
  time : float;
  ops_per_sec : float;
  mbs_per_sec : float;
  read_amplification_calls : float;
  read_amplification_size : float;
  write_amplification_calls : float;
  write_amplification_size : float;
}
[@@deriving repr]

type t = { perf : perf; stats : Stats.t }

let run ~entry_sz ~nb_entries f : t =
  let time, stats = with_stat f in
  let nb_entriesf = float_of_int nb_entries in
  let entry_sizef = float_of_int entry_sz in
  let bytes_read =
    stats
    |> Stats.get_by_name ~modul:Stats.Store.name ~stat:"io read"
    |> Stats.Func.get_count ~name:"nb_bytes"
  in
  let nb_reads =
    stats |> Stats.get_by_name ~modul:Stats.Store.name ~stat:"io read" |> Stats.Func.get_count
  in
  let bytes_written =
    stats
    |> Stats.get_by_name ~modul:Stats.Store.name ~stat:"io write"
    |> Stats.Func.get_count ~name:"nb_bytes"
  in
  let nb_writes =
    stats |> Stats.get_by_name ~modul:Stats.Store.name ~stat:"io write" |> Stats.Func.get_count
  in
  let read_amplification_size = float_of_int bytes_read /. (entry_sizef *. nb_entriesf) in
  let read_amplification_calls = float_of_int nb_reads /. nb_entriesf in
  let write_amplification_size = float_of_int bytes_written /. (entry_sizef *. nb_entriesf) in
  let write_amplification_calls = float_of_int nb_writes /. nb_entriesf in
  let ops_per_sec = nb_entriesf /. time in
  let mbs_per_sec = entry_sizef *. nb_entriesf /. 1_048_576. /. time in
  {
    perf =
      {
        time;
        ops_per_sec;
        mbs_per_sec;
        read_amplification_calls;
        read_amplification_size;
        write_amplification_calls;
        write_amplification_size;
      };
    stats;
  }

let pp ppf t =
  Format.fprintf ppf
    "Total time: %fs@;\
     Operations rate: %f op/s@;\
     Transfer rate: %f Mb/s@;\
     Read amplification in syscalls: %f@;\
     Read amplification in bytes: %f@;\
     Write amplification in syscalls: %f@;\
     Write amplification in bytes: %f" t.perf.time t.perf.ops_per_sec t.perf.mbs_per_sec
    t.perf.read_amplification_calls t.perf.read_amplification_size t.perf.write_amplification_calls
    t.perf.write_amplification_size

let pp_detailed ppf t = Fmt.pf ppf "%a@;@[<v 2>Detailed profiling:@;%a@]" pp t Stats.pp t.stats

let pp_perf = Repr.pp_json perf_t

let pp_json ppf t =
  Format.fprintf ppf "{\"performance\":%a,@;\"stats\" : %a}" pp_perf t.perf Stats.pp_json t.stats
