# Element writes through a qualified hash name persist past the routine

`sub h { %GLOBAL::h<a>++ }; h(); h(); say %GLOBAL::h.raku` printed `{}`. It now
prints rakudo's `${:a(2)}` (#10901). This is the hash counterpart of the
qualified-array `push` fix.

The subscript write ops (`=`, `++`/`--`, `OP=`, `:delete`, slices and nested
subscripts) wrote into the running frame's env, and the frame took the hash
with it when it returned. An increment of a hash that did not exist yet
vivified nothing at all, even at file scope. Before such an op on a
package-qualified `%` name, the VM now loads the `our_vars` container into env.
If there is no container yet, it vivifies one the way rakudo auto-creates an
undeclared package slot: a Scalar holding a Hash. After the op, the VM stores a
replaced container back. `GetHashVar` now reads a package hash from that store
first. A declared `our %h` reached as `%GLOBAL::h` or `%P::h` shares the same
container as the bare name.
