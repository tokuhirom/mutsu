# A module's qualified package names no longer fill the importer's env

`unit module JSON::Fast`, `package Example::A { … }` and their kin used to bind
the package under its own qualified name in the env of the program that loaded
the module — a module body runs there — so every frame env carried one entry per
qualified package the loaded modules declared. Those bindings are no longer made;
the package's kind record answers bareword and indirect lookups and the
enclosing package's stash instead.

That also fixes a stash divergence: without precompilation,
`use Example::A; use Example::B; Example::.keys` used to be empty, where rakudo
lists `A B C`.

A `Promise(supply { whenever … })` loop under `use Cro::HTTP2::RequestParser`
deep-copies 15% fewer env entries per iteration (3,765 → 3,195). This is the
fifth slice of ADR-0084 (#7817).
