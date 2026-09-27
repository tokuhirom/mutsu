# `Pkg::<$x>` reads one stash key instead of building the whole stash

A one-key package-stash read (`P::<$x>`, `P::{$k}`) used to materialize the
package's entire stash on every execution: the stash is not stored anywhere,
so `package_stash_value` rebuilt it by scanning the whole env, the `our` store
and every routine, class and role registry, and the subscript then picked one
entry out of it. Each read cost about 1 ms however few symbols the package had,
and grew with the env (#9171).

The compiler now fuses the stash with its subscript into
`GetPseudoStashKeyed`. For an ordinary package and a sigiled key, the VM asks
`package_stash_symbol` for that one entry, and the ordinary `Index` reads it
from a one-entry stash, so the result is exactly what the whole stash would
have given. The candidate keys come from `qualified_tail_index`, a process-wide
index from a member's bare name (`x`) to every interned qualified name ending in
it (`P::x`, `@P::x`, `Outer::P::x`, `P::x/2`). The index is recorded inside
`Symbol::intern_global`, the only place a symbol id is ever assigned, so it
can never miss a key that some store holds. The same approach already keeps
the capture-shape registry sound.

The whole-stash build and the keyed read call the same per-key membership
helpers (`env_stash_member`, `our_var_stash_member`, `routine_stash_member`),
so both apply the same rules for what a key means. Keyed stores into a named
package (`P::{$k} = ...`) use the same one-entry stash.

With 250 and with 1000 package variables, 20000 `P::<$p1>` reads cost 0.31 s
(about 15 µs each). Before, each read took about 1 ms, and that time grew with
the env. Whole-stash terms (`P::.keys`), unsigiled keys (sub-packages,
classes) and the pseudo-packages (`GLOBAL::`, `PROCESS::`, `DYNAMIC::`) still
build the whole stash.
