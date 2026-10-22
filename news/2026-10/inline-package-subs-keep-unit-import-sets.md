# An inline package's sub keeps the unit's import sets

`module M { sub f is export { g() } }` with `use N` (which exports `g`) at file
level, or inside the block, died with `Unknown function: g` when the importer
called `f` by its imported name (`import M; f()`), while `M::f()` worked. The
CHECK-time pre-registration of an inline package's routines compiled their
bodies on the fly with no lexical import chain, and `import M` exposes that
registration. The pre-registration now compiles them inside the import sets of
the unit's own top-level `use`s, as the unit's own compile does (#14387).
