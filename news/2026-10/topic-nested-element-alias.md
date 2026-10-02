# `given`/`with`/`without` alias nested and attribute element topics

`$_ = X without %f<a>[0]` (and `given @a[1][2]`, `given %!h<k>[0]`) died with
"Cannot assign to an immutable value" because only a single subscript of a plain
named variable was tagged as an element source. The `given` lowering now tags a
whole subscript chain (`TagElementSourcePath`) and attribute containers, and the
writeback autovivifies missing intermediate elements and writes attribute
containers back through `self`'s attribute cell.
