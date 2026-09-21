# Writable loop aliases now autovivify their source element

Mutating list methods such as `prepend` called through a writable loop alias
now update the aliased source element when it starts as `Any`/`Nil`. This fixes
the sparse-sheet construction used by `Arithmetic::PaperAndPencil`'s HTML
renderer.
