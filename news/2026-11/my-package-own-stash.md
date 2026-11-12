# Each `my package` declaration has a stash of its own

Two `my package` declarations with the same name, in two blocks or two
`EVAL`s, used to share their stash in mutsu. What one gained through
`P.WHO<…>` or `package_at_key` showed up in the other. In rakudo each
declaration is a fresh package with an empty stash.

A `my package` / `my module` is now stored under its declaration's own name
(`Name\0<decl_id>`), as a `my class` already was. The program still sees the
name it spelled. Several lookups now follow that storage name:

- a qualified type name, used as a term (`Holder::Inner`) or as a variable
  type (`my Holder::Inner $x`);
- a pseudo-stash read (`tmp::Foo::<$x>`);
- `import` of a lexical module.

Packages named `EXPORT` and `EXPORTHOW` keep their name, because the export
machinery finds them by it. A `my package` in a role body also keeps its
name, because each composition of the role instantiates it.

This also fixes two gaps lexical classes already had:

- a nested type of a `my class` as a variable type (`my C::D $x`);
- the `:ver`/`:auth` adverbs of a `my class` or `my module`, which `.^ver` /
  `.^auth` did not report.

CompUnit::Util 0.7.0's `t/01-utils.rakutest` passes 17/17 (#17477, #15371).
