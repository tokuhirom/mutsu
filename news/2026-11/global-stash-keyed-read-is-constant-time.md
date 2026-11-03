# `GLOBAL::<$x>` reads one key instead of building the whole root stash

`GLOBAL::<$x>`, `<@x>` and `<%x>` built the whole `GLOBAL::` stash on every read: a scan of the
env, the `our` store and the exported variables, so a read cost O(v) in the size of the program.
With 500 `our` variables a loop of 1500 reads took 8.1 s (debug build) and with 2000 it took 28.3 s;
the same loop now takes 0.57 s and 0.86 s, flat in the number of variables. The root stash gets a
sigiled variable from three stores only (the `our` store, the env plus the modules' top-level
package symbols, the exported variables), so the keyed read asks the same membership helpers the
whole build uses, for the four spellings a root symbol can have (`x`, `GLOBAL::x`, `@x`,
`GLOBAL::@x`). `MUTSU_CHECK_GLOBAL_STASH=1` makes a debug build compare every such read with the
whole stash; the `t/` files that touch `GLOBAL`, `OUR::`, `.WHO` or `::()` (363 files) agree.

A routine or type key (`GLOBAL::<&f>`, `GLOBAL::<Foo>`) and the other pseudo-packages still build
the stash; #9171 stays open for them.
