# A `=finish` line inside a heredoc is text

`split_finish_section` cut the unit at the first `\n=finish` line, even one inside a
heredoc body, so `q:to/END/` holding a `=finish` line failed to parse
(`Undeclared routine: q:to`). It now skips heredoc bodies (`:to/X/`, `:to<X>`,
`:to"X"`, `<<X`) when looking for the marker, as rakudo does. Found with
static's `t/14-cli.rakutest`.
