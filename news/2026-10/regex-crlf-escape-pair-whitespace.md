# `\r` and `\n` separated by whitespace no longer match CRLF

CR LF is one grapheme, so `\r` followed by `\n` only denotes the pair when the
two escapes are adjacent and no whitespace follows the `\n`, as in rakudo.
`"ab\r\ncd" ~~ /b \r\n c/` and `/\r \n/` are now `False` (#12828); the literal
re-joining pass learned where the pattern skipped insignificant whitespace.
