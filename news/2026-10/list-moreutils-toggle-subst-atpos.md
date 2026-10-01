# List::MoreUtils: toggle regex blocks, s/// topic aliasing, AT-POS rw arguments

Three interpreter gaps found by the List::MoreUtils suite. `toggle` now boolifies a
regex-returning condition block (`{ /foo/ }`) against its lexical topic instead of
treating the Regex object as true. A block containing `s///` or `tr///` is now marked
as writing its topic, so calling it on a variable aliases `$_` and writes through.
`f(@a.AT-POS($i))` is compiled like `f(@a[$i])`, so an `is rw` parameter binds the
element container. `t/after`, `t/after_incl`, `t/apply` and `t/pairwise` now pass.
