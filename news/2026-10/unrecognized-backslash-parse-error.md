# An unrecognized backslash escape is rejected at parse time

`"a\zb"` and the other interpolating quote forms used to compile to a call of the magic-named
`__mutsu_unknown_backslash_escape` that died only when the string was evaluated, so a branch that
never ran hid the error. As in rakudo, the unit is now rejected with
`X::Backslash::UnrecognizedSequence` when it is parsed. `process_content_with_flags` and the
`\q[...]` / `\qq[...]` / `\qw[...]` helpers it calls are fallible end to end; an interpolating
heredoc body is checked at parse time; the compiler and the `s///` replacement plan, which read quote
text after the parse, raise the same exception. Deleted: the marker call, its runtime builtin and the
`convert` arm that rendered it (#7564).
