# A compile error inside `EVAL` is reported against the EVAL's own text

An uncaught compile error raised while parsing an `EVAL`'d string was printed
as `===SORRY!=== Error while compiling script.raku ... at script.raku:1` with
the calling file's first line as the excerpt: the error carried the EVAL's
line but not its unit or text. It now carries both, so the report names
`EVAL_N`, the line within the EVAL text and that text's excerpt, as rakudo
does. `EVAL` also stopped trimming leading blank lines off its argument, which
made `.line` count from the first non-blank line (#18870).
