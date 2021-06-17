export F=50
export N=1_000_000
dune exec -- ./bench/bench.exe --minimal --clear --fanout $F --page 4700 -n $N --with-profiling -v
python bench/graph.py _bench/replace_random --with-log
open _bench/replace_random/stats.png