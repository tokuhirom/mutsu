# A tail bare block that reaches `when` ends at the matched clause

`{ when True { say 1 }; say 2 }` as the last statement of a file, or as the whole
body of a `sub` or pointy block, died with "succeed without when clause" (or
unwound out of the routine): the inlined tail block had no `SucceedBarrier`.
Such a block now goes through `DoBlockExpr`, which absorbs the `when`'s succeed
and carries its value, as it already did in statement position. Found by
running `raku-doc` code blocks against mutsu and `raku`.
