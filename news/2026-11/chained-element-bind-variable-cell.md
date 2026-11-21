# Binding a variable through a vivified chained subscript shares its Scalar

`%g<p><q> := $x` shares `$x`'s container in Rakudo whether `%g<p>` already exists or the bind
autovivifies it. mutsu shared it only through an existing path: when the bind created an
intermediate level, the leaf received a readonly copy of the value, so a later `$x = 7` was not
seen through `%g<p><q>` and `%g<p><q> = 9` died with "Cannot assign to an immutable value"
(#18534). The 3+-level store (`IndexAssignDeepNested`) now seats the source's cell and rebinds
the source to it on either path. The local test that pinned the old behaviour was checked
against rakudo 2026.11 and updated; its other divergences from rakudo are filed separately.
