RakuAST round-tripping now preserves indirect method calls used as assignment
lvalues, including interpolated method names. Dotty assignment also retains
quoted method calls such as `$text .= "uc"()` and lowers them through the
existing read-modify-write path.
