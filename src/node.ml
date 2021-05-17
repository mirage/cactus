include Node_intf

module Make : MAKER =
functor
  (Params : Params.S)
  (Store : Store.S)
  (Key : Data.K)
  ->
  struct
    module NodeFmt = Vertex.NodeMake (Params) (Store) (Key)
    module CommonFields = Field.MakeCommon (Params)
    module Address = CommonFields.Address

    type key = Key.t

    let encode_address = Address.of_int

    let decode_address = Address.to_int

    type t = { store : Store.t; node : NodeFmt.t; address : Store.address }

    let pp ppf t = NodeFmt.pp ppf t.node

    let create store kind =
      let address = Store.allocate store in
      { store; node = NodeFmt.create store kind address; address }

    let load store address = { store; node = NodeFmt.load store address; address }

    let self_address t = t.address

    let depth t = NodeFmt.depth t.node

    let overflow t = NodeFmt.length t.node > 2 * Params.fanout

    let will_overflow t = NodeFmt.length t.node >= 2 * Params.fanout

    let underflow t = NodeFmt.length t.node < Params.fanout

    let split t =
      let address = Store.allocate t.store in
      let k, node = NodeFmt.split t.node t.store address in
      (k, { store = t.store; node; address })

    let find t key = NodeFmt.find t.node key |> decode_address

    let add t key address = address |> encode_address |> NodeFmt.add t.node key

    let delete t key = NodeFmt.remove t.node key

    let migrate kvs depth = NodeFmt.migrate kvs depth

    let length t = NodeFmt.length t.node

    let iter t func =
      let f key address = address |> decode_address |> func key in
      NodeFmt.iter t.node f

    let fold_left func acc t =
      let func acc (key, address) = func acc key (address |> Address.to_int) in
      NodeFmt.fold_left func acc t.node
  end
