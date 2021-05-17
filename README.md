# On disk implementation of B-🌲

The code will use eventually the format specified in [file-format.md](../../doc/file-format.md). Currently, the internal nodes do not use key pointers, but instead use the same format as leaves. The program structure reflects this as ```node_fmt.ml``` is basically a ```cp``` of ```leaf_fmt.ml```.

![Program structure](../../images/fformat/program.png)

## Understanding the code design

### Workflow and vocabulary

This work implements **btrees**, a data structure for **key**-**value** association, with low IO footprint.  B-trees are composed of internal nodes, simply called **nodes**, and **leaves**. Nodes and leaves are filled with keys. If ```n``` is the number of keys of a node/leaf, then ```n``` verfies

```
fanout ≤ n ≤ 2 * fanout
```

where ```fanout``` is a btree hyperparameter. When two many keys are added, the node/leaf is said to **overflow** and balancing operations must be conducted (namely, **splitting**). When two many keys are removed, the node/leaf is said to **underflow** and balancing operations must be conducted (namely, **merging**).

Each node/leaf usually lives in a single disk **page**. A page can be referenced with an **address**, and the **store** is in charge of retrieving a page from its address. Inside a page, a single byte can be referenced with a **pointer** (usually, the offset from the start of the page). Sometimes, a page is **full** but the underlying node or leaf still is not overflowing. We then allocate an additional page for this node, called an **overflow page** (the naming is a bit unfortunate, as this is not related to the node overflowing, but rather, in a sense, the page overflowing), and we add the address of this overflow page to the main page. Theoretically, we could recursively construct an unbounded chain of pages like this, although this is undesired (and would mean that the fanout is not well tuned to the device page size). Naturally, the overflow page constructed for a node is called an **overflow node**, and for a leaf an **overflow leaf**.

Nodes and leaves behaves much like an association map in their own right. Leaves map keys to values, and nodes map keys to other nodes/leaves, i.e. to addresses.

### File format implementations

Modules ```Leaf_fmt``` and ```Node_fmt``` provide an abstraction of the low level details of translating a node/leaf to disk writable contents. Currently, the onus is on them to ensure that the in-memory handle ```t``` they provide is correctly reflecting the disk content they represent.

There are two levels of abstraction that help the program's correctedness (although do not guarantee it). 
First, two functions ```low_read``` and ```low_write``` are implemented inside each ```*_fmt``` module, which abstracts IOs with the leaf/node keys and values. In short, every other functions of the module *are not allowed* to use the low level ```Page.read``` and ```Page.write```. Notably, only ```low_write``` has to be concerned with updating both the disk content and the in-memory handle ```t```. Similarly, a function ```rewrite_header``` is implemented to abstract updating header fields on disk, and must be called after each change to a header.
Second, two additional functions ```read``` and ```write``` are implemented inside each module, which act at the scale of entries (i.e. keys, values, and possible additional information e.g. flags), instead of the scale of bytes. This is the only scale the program should ever be concerned with, and so while ```read``` and ```write``` call ```low_read``` and ```low_write```, no other function is allowed to.

At the entry level, and header level, there can be many fields to keep track of. All of them are abstracted under their own dedicated module (basically containing an ```encode``` and a ```decode``` function). Some header fields are common to every page and their dedicated module live in the shared ```header.ml``` module. Among those shared fields is notably ```kind``` which designates the page to be a leaf or a node (or an overflow leaf or an overflow node). This allows ```Store``` to implement a universal ```Store.Page.Header``` which is in particular able to determine the ```kind``` of a page. This is necessary to know with what module to load a page, given an address.


## Milestones

- Comparison with the current implmentation
	- match ```mirage/index``` API
	- implement migration from ```index/data``` to ```b.tree```
- Feature support
	- Data consistency and integrity
	- Concurrency
	- Error detection
- Optimisation
	- Code refactoring
	- Do the todos
	- Door open to various optimisations


## To do

- elegantly handle loading an empty page (on creation, a '\000' filled header is incorrectly interpreted as a Overflow_leaf and goes straight to the volatile cache).
- lru cache
- migrate : utiliser des overflow_page si besoin
- dans Store.Page.write c'est très costly de travailler avec des strings plutôt que des bytes, puisqu'il faut tout recopier à chaque fois. Peut être qu'on a intérêt à faire le gros switch et travailler partout avec des ```bytes``` plutôt.
- all the dead flags could be gathered into a single leading chunk of bytes (which also reduces by 8 the number of necessary bytes), in for instance the header.
- instead of rewrite all the time, we could ask a rewrite on flush ?
- names in stats should be variants and not string
- passe rootdir en argument à ```Tree.create``` et ne pas ```chdir```
- !! promoted pourrait être un entry dead dans l'état actuel, ça ne va pas
- la taille de l'encodage de key n'est pas garantie dans ```*_fmt.ml```.
- rewrite-header should take the new header as argument and handle both the in-memory and on disk changes.
- virer les conneries de header dans Params, tout mettre dans Header
- je ne veux plus voir de StringHeaderField
- get free from the "no first \000 character" constraint
- change tbl.mli header comment
- good way to compare between encoded values and header fields
- les load pourraient load à partir du Encoded.encoded plutôt que pointer, comme ça on peut charger une fois, trouver le kind avec header, puis charger avec le bon module
- uniformiser les noms
- maybe the decoding function of some header fields, e.g. ```common_prefix```, should have a different type to accept an offset (of padding). This allows for the ```encoded``` type to have more leeway, but possibly falls back into the same issue that we have less guarantee upon the consistency of the encoded length. Still, the current alternative of ```Encoder.sub``` to an offset has basically the same caveats whilst being less elegant.
- uniformise ```x_to_y``` versus ```y_from_x```, and ```init``` versus ```create```
- Lazy loading : currently the whole leaf/node *and their overflow pages* are loaded at once. The overflow pages should only be loaded when necessary.
- I'm not protecting the difference leaf and overflow_leaf with OCaml's type system. This is convenient to not have to duplicate every function, but is less secure.
- ensure that values have uniform length, equal to ```Fformat.value_sz```
- ensure that keys have uniform length, equal to ```Fformat.key_sz```
- In addition, various ```(* TODO : ... *)``` spread in the code
- Currently the onus is on the file format implementation to ensure that the in-memory representation of the node/leaf is equivalent to the on disk content, i.e. each update must change both the disk content and its in-memory representation. To try and ensure this, my implementation abstracts the IO interface with ```read``` and ```write``` custom functions. It is then only necessary to check that ```read``` and ```write``` preserve the invariant. Still, nothing prevents the other functions to call ```Page.read ``` and ```Page.write ``` rather than ```read``` and ```write```. It would be nice to prevent this with the type checking system.

- ✅ @ngoguey : Pas de matching sur des string, utiliser des variants polymorphes plutôt (voire des variants tout court).
- ✅ ```README``` at the root
- ✅ In leaves, shift only up to the next dead entry
- ✅ Use ```Format``` to get some at least decent pretty_printer
- ✅ Node headers are not created with the correct kind, must change ```node_intf``` for that.
- ✅ modularise every header field (```kind```, ```checksums```, etc), useful to 1 abstract the type and encodings, and 2 have more guarantee that we preserve the invariant of fixed size encodings
- ✅ In ```Leaf_fmt``` the functions related to ```kind``` are specific to leaves and cannot be used for nodes
- ✅ ```kind``` encoding should use exactly ```Fformat.tree_height_sz``` bytes
- ⏳ assert checks everywhere [done in ```left_fmt```]
- ⏳ assert instead of ```Not_found``` / ```false``` in  ```find``` et ```mem``` [done in ```leaf_fmt```]
- ⏳ factorise ```entry_number``` et ```dead_entry_number``` [done in ```leaf_fmt```]
- ✅ ```read``` and ```write``` should act at the entry scale, not the byte scale [done in ```leaf_fmt```]
