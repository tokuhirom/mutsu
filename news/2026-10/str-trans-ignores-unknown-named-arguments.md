# `Str.trans` ignores named arguments that are not `:s`/`:d`/`:c`

`"abcdefg".trans("\t" => '  ', :g)` rewrote the `g` to `T`: the `:g` named argument was handed to
the translation as a positional `g => True` pair. Only the real adverbs (`:s`/`:squash`,
`:d`/`:delete`, `:c`/`:complement`) are forwarded now; anything else is ignored as in Rakudo.
Terminal::UI's `t/03-pane.rakutest` (its `sanitize` helper passes `:g`) now matches Rakudo.
