# A pulled `gather` body forgot the package it was written in

`Concurrent::Trie`'s `entries` method returns a `gather` block whose body
calls `entry-walk`, a `sub` lexical to the class body. Under mutsu, calling
`.list` (or any other forcing operation) on that `Seq` from outside the class
died with `Unknown function: entry-walk`, even though the same call worked
fine when made directly, without going through `gather`, from inside the
method.

The root cause: a `gather`'s body is compiled once and then run lazily,
potentially long after the frame that created it (and that frame's
`current_package`) has returned control to a caller in an entirely different
package. Every one of mutsu's several lazy-forcing paths — the VM-native
bytecode-resuming coroutine (`force_lazy_list_vm`/`_vm_n`) and the older
tree-walk bridges (`force_lazy_list_bridge`, `force_lazy_list_prefix_bridge`)
— ran the body under whatever package happened to be current at the *pull*
site, not the package it was *written* in. A bare (unqualified) call inside
the body resolves against `current_package` at run time, so a class-body
lexical `sub` referenced from a `gather` written inside one of that class's
methods became unreachable as soon as the `Seq` was forced from mainline code
or any other package.

This already had a narrower fix for the analogous compunit-private-routine
case: `exec_make_gather_op` captures `current_unit` into the gather's env
snapshot (`__mutsu_gather_unit`), and every force path restores it for the
duration of the pull, so a top-level `my sub` stayed reachable
(`t/modules/gather-module-lexical-helper.t`, from the File::Find fix). The
same capture-and-restore now happens for `current_package`
(`__mutsu_gather_package`), across all four force paths.

Pinned by `t/collections/lazy-seq/gather-class-lexical-sub.t`. Found via
`ecosystem-dist-roulette` while making `Concurrent::Trie`'s own test suite
pass: `t/basic.rakutest` goes from `red` (dying on the very first `is-deeply`
that forces `.entries`) to full parity, 42/42 assertions.
