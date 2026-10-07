# Non-destructive regex forms preserve smartmatch operands

`TR///` no longer writes its translated copy back to the left operand when it
appears on the right of `~~`. `S///` and `TR///` smartmatches now agree with
Rakudo by returning `False` while leaving the source string unchanged. The
transliteration tests pin the distinction from destructive `tr///`.

Fixes #12174.
