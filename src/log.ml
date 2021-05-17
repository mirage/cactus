let src = Logs.Src.create "btree" ~doc:"logs btree's events"

module Log = (val Logs.src_log src : Logs.LOG)

include Log

module Tag = struct
  type kind = Stats [@@deriving repr]

  let kind_tag : kind Logs.Tag.def =
    Logs.Tag.def "log kind" ~doc:"Kind of the logged info" (Repr.pp kind_t)

  let v () = Logs.Tag.empty

  let kind_it kind tagset = tagset |> Logs.Tag.(add kind_tag kind)
end
