# A routine a later top-level `use` imports is callable from a line above it

`use` is compile-time in Raku, so `say greet4("x"); use LateOps3;` calls the
imported `greet4`. mutsu bound the import from the `use`'s line on and died
with `Unknown function: greet4` for the call above it. That rule is still
right when the name already meant something (a core routine, an earlier
`sub`), so the call above a unit-level `use` now carries a fallback import
chain that answers only a name nothing else resolves at run time.
