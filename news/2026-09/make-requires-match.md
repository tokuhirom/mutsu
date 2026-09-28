# `make` throws when `$/` is not a Match

`make "x"` outside a successful match used to succeed silently. It now dies with
`The make function expects $/ to contain a Match, but it contains Nil`, like Rakudo
(`Type/Match.rakudoc`). Pinned by `t/grammar/make-requires-match.t`.
