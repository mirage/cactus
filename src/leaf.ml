include Leaf_intf

module Make : MAKER =
functor
  (Params : Params.S)
  (Store : Store.S)
  (Key : Data.K)
  (Value : Data.V)
  ->
  struct
    module LeafFmt = Vertex.LeafMake (Params) (Store) (Key) (Value)

    type key = Key.t

    type value = Value.t

    type t = { store : Store.t; leaf : LeafFmt.t; address : Store.address }

    let pp ppf t = LeafFmt.pp ppf t.leaf

    let init store address = { store; leaf = LeafFmt.create store Field.Leaf address; address }

    let create store =
      let address = Store.allocate store in
      init store address

    let load store address = { store; leaf = LeafFmt.load store address; address }

    let self_address t = t.address

    let overflow t = LeafFmt.length t.leaf > 2 * Params.fanout

    let will_overflow t = LeafFmt.length t.leaf >= 2 * Params.fanout

    let underflow t = LeafFmt.length t.leaf < Params.fanout

    let split t =
      let address = Store.allocate t.store in
      let k, leaf = LeafFmt.split t.leaf t.store address in
      (k, { store = t.store; leaf; address })

    let find t key = LeafFmt.find t.leaf key

    let mem t key = LeafFmt.mem t.leaf key

    let add t key value = LeafFmt.add t.leaf key value

    let delete t key = LeafFmt.remove t.leaf key

    let iter t func =
      let f key value = func key value in
      LeafFmt.iter t.leaf f

    let length t = LeafFmt.length t.leaf

    let migrate kvs = LeafFmt.migrate kvs Field.Leaf
  end
