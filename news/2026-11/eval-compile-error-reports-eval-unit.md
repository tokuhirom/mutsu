# An uncaught compile error inside `EVAL` names the EVAL unit

A compile error raised inside `EVAL` (or by `Str.AST` failing to parse) used to
be reported at the calling file's line 1, quoting the first line of that file:
`===SORRY!=== Error while compiling x.raku … at x.raku:1 ------>say 1;`
(#18870). The exception object already knew better, since its `.filename` was
`EVAL_N` and its `.line` was the line within the EVAL text. But the
uncaught-error renderer took the file name and the excerpt from the running
program.

The parse failure now carries its unit's name and text, the way a failing
`use`d module's does. The report reads `Error while compiling EVAL_0`,
`at EVAL_0:2`, and quotes the offending EVAL line.
