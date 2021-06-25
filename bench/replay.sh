export DATA=~/Documents/data
export C=100

cd ~/Documents/irmin
opam switch irmin && eval $(opam env)
rm -rf $DATA/sess2_t1nest_a
MEMTRACE=trace.ctf dune exec -- ./bench/irmin-pack/tree.exe --mode trace $DATA/trace.repr --ncommits-trace $C --keep-stat-trace --path-conversion none  --artefacts $DATA/sess2_t1nest_a -v
mv small-trace.btree.repr $DATA/$C-trace.btree.repr

cd ~/Documents/btree
opam switch default && eval $(opam env)
MEMTRACE=trace.ctf dune exec bench/replay.exe -- -vv $DATA/$C-trace.btree.repr

opam switch memtrace && eval $(opam env)
memtrace-viewer trace.ctf