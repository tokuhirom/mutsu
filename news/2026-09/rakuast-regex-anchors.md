`RegexTree` now preserves Raku's string and line anchors (`^`, `$`, `^^`, and
`$$`) through `.AST` and constructed-tree `EVAL`. The four anchors render as
their corresponding `RakuAST::Regex::Anchor::*` nodes and lower through the
existing regex matcher.
