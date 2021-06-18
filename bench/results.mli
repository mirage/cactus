module Stats = Btree.Private.Stats

type perf = private {
  time : float;
  ops_per_sec : float;
  mbs_per_sec : float;
  read_amplification_calls : float;
  read_amplification_size : float;
  write_amplification_calls : float;
  write_amplification_size : float;
  tree_density : float;
}

type t = { perf : perf; stats : Stats.t }

val run : entry_sz:int -> nb_entries:int -> (unit -> 'a) -> t

val pp : t Fmt.t

val pp_detailed : t Fmt.t

val pp_json : t Fmt.t

val pp_perf : perf Fmt.t
