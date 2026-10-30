# Every parse entry returns the round-tripped tree; the unspelled parse is gone

`parser::parse_program`, `parse_fragment` and `parse_program_recovering` (and with them
`parse_dispatch::parse_source`, EVAL's `:check` mode and the analysis frontend) now parse with
spellings kept and run `parse -> RakuAST -> lower`, exactly like the main program and every
`use`d module. Only the conversion pipeline (`parse_program_spelled`), the module declaration
scan made in the middle of another unit's parse (`parse_program_partial_spelled`) and nested
block fragments see the raw tree.

That made the `spelled::keeping()` thread-local flag meaningless, so it is deleted together with
every else-arm that lowered a construct at parse time (assignments, compound assignments, `.=`,
set/reverse/meta assignments, `supply`, inline `subset`, `start` bodies, hyper assignment, the
`for`/conditional finishers). `lower` strips the spellings that shared builders hand back
(`rakuast/unspell.rs`), and `ast_visit::walk_parsed` visits a wrapped declaration as a statement
(a kept `class X is export` was invisible to the declaration scan).
