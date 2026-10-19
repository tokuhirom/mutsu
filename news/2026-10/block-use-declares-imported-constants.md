# A block's `use` declares the constants it imports, and callees no longer see its types

A `use` inside a block now declares the constants it imports as the block's
own bindings, as it already does variables (ADR-8751 stage 1, slice 4). An
imported `constant LIMIT` hides an outer `LIMIT` inside the block, and a
closure taken there keeps it.

A routine written at the top of the file no longer sees a block's imports
when the block calls it. Before, only a routine that looked names up by
string was shielded; now `sub make { Widget.new }` called from
`{ use Widgets; make() }` builds the program's own `Widget`, not the
imported one, as in rakudo.
