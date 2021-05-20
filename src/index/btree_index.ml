include Btree_index_intf

module Make (Key : Index.Key.S) (Value : Index.Value.S) = struct
  module Key : Btree.Input.Key with type t = Key.t = struct
    include Key

    let decode s = decode s 0
  end

  module Value : Btree.Input.Value with type t = Value.t = struct
    include Value

    let decode s = decode s 0
  end

  include Btree.Make (Key) (Value) (Btree.Input.Default.Size)

  exception NotInBtree of string

  let try_merge _t = raise (NotInBtree "try_merge")

  let merge _t = () (* raise (NotInBtree "merge") *)

  let is_merging _t = raise (NotInBtree "is_merging")

  let sync _t = raise (NotInBtree "sync")

  let close ?immediately t =
    ignore immediately;
    flush t

  let filter _t _cond = raise (NotInBtree "filter")

  let replace ?overcommit t key value =
    ignore overcommit;
    add t key value;
    flush t

  let v ?flush_callback ?cache ?fresh ?readonly ?throttle ~log_size root =
    ignore flush_callback;
    ignore fresh;
    ignore readonly;
    ignore throttle;
    ignore log_size;
    match cache with None -> create root | Some cache -> create ~cache root

  let flush ?no_callback ?with_fsync t =
    ignore no_callback;
    ignore with_fsync;
    flush t
end
