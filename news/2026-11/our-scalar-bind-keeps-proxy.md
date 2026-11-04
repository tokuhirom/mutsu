# `our $x := <Proxy>` binds the Proxy instead of fetching it at the declaration

`our $kThing is export := cglobal(LIB, "kThing", T)` (NativeCall's lazy `Proxy`)
FETCHed the proxy when the declaration ran and stored the value back through
`STORE`, so a module whose native library is not installed died while it loaded
-- rakudo loads it and only fails when the variable is read. The global store
after a scalar `:=` bind now keeps the bind context, as it already did for a
bound `@`/`%` container. deredere's `t/10-correctness.t` (Gumbo::Binding) now
loads and passes like rakudo.
