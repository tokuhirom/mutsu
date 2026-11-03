# RakuAST: origins inside the subscript of an interpolated variable

In `"@r[0]"` / `"%h{$k}"` the terms inside the subscript, and so the `SemiList` and statement
around them, now carry the ranges Rakudo gives them. The string parser reads the subscript as an
expression without positions, so its text is parsed again as a fragment that keeps its spelled
terms, moved to where it sits in the unit, and the ranges of the converted fragment are copied
onto the nodes of the same shape (`fragment_origins.rs`). Part of #15780.
