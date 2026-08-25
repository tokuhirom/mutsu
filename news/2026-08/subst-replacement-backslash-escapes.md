# `s///` replacements honour the full backslash-escape set

`$_ = '18:38'; s/(\d+)\:(\d+)/{$0 % 12}\:$1 {$0 < 12 ?? 'AM' !! 'PM'}/` printed
`6\:38 PM` — the escaped literal colon kept its backslash. The replacement's
escape handling (`normalize_subst_replacement`) recognized a short fixed list
(`\n`, `\t`, `\\`, `\x`, ...) and passed anything else through with the
backslash still attached, so `\:`, `\/`, `\{` and `\c[NAME]` were all wrong.

Raku's rule, which `raku-doc/doc/Language/quoting.rakudoc` states and which the
`"..."`/`qq//` parsers in mutsu already implemented, is that a backslash before
any non-alphanumeric character yields that character, on top of the named
escapes (`\x[41]`, `\o[..]`, `\c[LATIN SMALL LETTER Z]`, `\c10`, ...).

The fix was not to extend the list: the replacement is a `qq` quote, so it now
goes through the one `qq` parser and inherits the whole escape set for free.
See [subst-replacement-is-a-qq-quote.md](subst-replacement-is-a-qq-quote.md) for
the mechanism.

One related `qq` gap surfaced and was fixed on the way: `process_content_with_flags`
(what `qq//` and heredocs use for closure interpolation) only accepted a single
*expression* inside `{ ... }`, so `qq!{ my $t = 1; $t + 1 }!` rendered as literal
text with its variables blanked. It now uses the same `parse_braced_closure_body`
helper the `"..."` parser does, which tries a statement list first — so
multi-statement blocks work in `qq//`, heredocs, and substitution replacements
alike.

Pinned by `t/subst-replacement-interpolation.t`.
