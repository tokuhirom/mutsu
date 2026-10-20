# A block's imported enum values are hidden from its callees

A routine written outside a block no longer sees the enum values the block's
`use` imported (#14214, ADR-8751). `{ use M; outside() }`, with
`sub outside { ::('Green') }`, found `M`'s exported `enum Color <Red Green>`
value before; rakudo does not.

The import writes an enum value under its own key namespace
(`__mutsu_enum_bare_Green`, #7914). `hide_unit_names_from_frame`, which hides
a block's imported names from a top-level routine the block calls, took only
variable-shaped keys and the module-merge grant. It now takes enum-value keys
too.
