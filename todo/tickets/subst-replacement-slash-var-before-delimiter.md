# `$/` written directly before a substitution's closing delimiter doesn't lex

Found while making the `s///` replacement a real `qq` quote
(`news/2026-08/subst-replacement-is-a-qq-quote.md`). Everything else on that
survey reached parity; this one is in the *delimiter scanner*, not the
interpolation grammar, so it was left out of that change.

## Repro

```raku
my $s = 'ab'; $s ~~ s/(a)/[$/]/;   say $s;   # raku: [a]b
my $s = 'ab'; $s ~~ s:g/<[ab]>/$//; say $s;  # raku: ab
```

- raku: `[a]b` / `ab`
- mutsu: `===SORRY!=== Confused. expected statement` / `Runtime error: Regex not terminated.`

Both work today when the `$/` is nested inside braces (`s:g/<[ab]>/%h{$/}/` is
fine), and when the `$/` carries a postfix (`$/.chars()`, `$/[0]`, `$/<k>`).

## Root cause

`scan_to_delim_replacement` (`src/parser/primary/regex/scan.rs`) scans the
replacement for its closing delimiter. It has a special case for `$` immediately
followed by the close delimiter, but only consumes the delimiter as part of `$/`
when a `[`, `.` or `<` postfix follows it:

```rust
} else if c == '$' && !is_paired && input[i + 1..].starts_with(close_ch) {
    let after_delim = &after[close_ch.len_utf8()..];
    if after_delim.starts_with('[') || after_delim.starts_with('.') || after_delim.starts_with('<') {
        chars.next(); // skip the delimiter char (it is part of $/)
    }
}
```

So a bare `$/` before `]` or before the real closing `/` ends the replacement one
character early.

## Why it isn't a one-liner

Rakudo lexes `$/` as a term unconditionally, which is why the general case works
there. Doing the same here means deciding, for every `$` + close-delimiter pair,
whether the user meant the `$/` variable or an anchor/literal `$` followed by the
end of the replacement — and the answer differs between a `/`-delimited
substitution and a bracketing one, and between the pattern half (where a
trailing `$` is the end-of-string anchor) and the replacement half. Getting it
wrong turns a currently-working `s/foo$/bar/` into a parse error, so it wants a
deliberate rule plus its own test matrix rather than an extra `||` in the
condition above.

## Affected files

- `src/parser/primary/regex/scan.rs` — `scan_to_delim_replacement`, and the
  matching disambiguation in `scan_to_delim`.
