# `.^set_name` no longer races other threads on a shared mixin type

The one Cro::HTTP SIGSEGV in CI (#14765) was a memory-safety bug in mutsu's own
state, not in the test. `.^set_name` on a role composition writes the name onto
the composition-keyed `.WHAT` node (ADR-0060). Every thread spawned after the
node was made shares that node. NativeCall's `CArray[T]` and `Pointer[T]` run
`^parameterize`, and that renames the node on every call. `Cro::TCP::NoDelay`
runs `CArray[int32]` for each socket, so on the first connection the server's
accept thread and the client's connect thread both renamed `CArray[int32]` at
once. The name was inserted into the node's overrides map through
`gc_contents_mut` with no lock, while other threads read that map. Two inserts
rehashed it under each other, and the process crashed before the test printed
its first `ok`.

A debug build looping the test under load reproduced the crash. The crash report
pointed at `MixinOverrides::insert` in `mop_set_name`, on a pool thread, reached
from a `.then` callback. The name now has its own `RwLock`ed slot,
`MixinOverrides::type_name`. `.^set_name` writes it, and skips the write when the
name is unchanged. `.^name`, a type object's `.raku`/`.gist` and the
constant-alias resolver read it. The two `unsafe` `gc_contents_mut` writes are
gone. `t/concurrency/concurrent-mixin-set-name.t` renames shared compositions
from six threads released together, and parameterizes NativeCall types the same
way.

The crash also showed why CI kept no backtrace. The `test-suites` job collected
`tmp/crash` *before* the battery gate ran, so the battery's report was never
uploaded. The gate now prints the crash report of any interpreter that dies of a
signal into its own log.
