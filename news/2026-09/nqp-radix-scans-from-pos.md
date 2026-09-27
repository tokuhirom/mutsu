# `nqp::radix` scans from `$pos` instead of copying the whole string

`nqp::radix($base, $s, $pos, $flags)` used to stringify `$s` and collect
every grapheme into a `Vec<char>` on each call, whatever `$pos` was. A
tokenizer that calls it at successive positions — the way Rakudo's own
number parsing and hand-written NQP parsers do — was therefore O(n^2) (#9131).

The op now walks forward from `$pos` over the string's cached grapheme index
(`str_prim::chars_from`), and accumulates the result without buffering the
digits, so a call costs O(k) in the digits it consumes, matching MoarVM.
`scripts/nqp-complexity-check.sh radix` went from a ratio of 3.67
(0.21 s -> 0.77 s at N = 10k -> 20k) to 1.76 (0.004 s -> 0.007 s).

The digit test is now the one `parse-base` uses, extended with the fullwidth
Latin letters, so `nqp::radix` accepts any Unicode `Nd` digit (`"٣"`) and
fullwidth letters (`"Ｚ"`) the way MoarVM does.
