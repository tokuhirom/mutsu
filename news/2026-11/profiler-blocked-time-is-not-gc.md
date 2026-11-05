# The profiler reports blocked waits as `blocked`, not `gc`

`--profile` charged every native blocking wait (`sleep`, a thread join, a socket
read, a child-process wait) to the `gc` excluded region, so a join-heavy run
printed `EXCLUDED gc 222.6s` of a 227 s wall clock although `MUTSU_GC=off`
saved only ~5%. Those waits now have their own `blocked` region; `gc` is only
collector time and stop-the-world parks (#15577).
