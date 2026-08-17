# Quantified scalar interpolation in regex now matches the whole value

A quantifier attached to an interpolated scalar (`m/ $s? /`, `m/ $s+ /`)
previously never matched. `interpolate_regex_scalars` splices the scalar's
escaped value into the pattern text, wrapped in
`NON_DECLARATIVE_INTERP_MARK` sentinels, and leaves a following `?`/`+`/`*`
in place for the structural parser. The tokenizer treated the marks as a
no-op toggle and built one `Literal` token per spliced char, so a trailing
quantifier landed on whatever the loop's `chars.peek()` saw immediately
after the mark toggled off — never the interpolated span itself. For a
single-char value like `$s = "z"` this silently attached to nothing (the
mark right after `z` blocked the peek), and for a multi-char value like
`$s = "abc"` it could at best have quantified only the last spliced char,
never the whole "abc" atom the way Raku does.

Fixed by tracking the token-list index where an interpolation span starts
(the opening mark) and, at the closing mark, trying to consume a trailing
quantifier (`*`, `+`, `?`, `**N..M`, `**{code}`, plus the frugal `?`
modifier). When one follows, the span's tokens are wrapped into a single
`RegexAtom::Group` (or, for the common single-char case, the quantifier is
applied directly to that one token) so the quantifier binds to the entire
interpolated value, matching Raku's "one atom" semantics. When no
quantifier follows, parsing is unchanged — each char of the span still
becomes its own literal token as before, which was already correct for the
unquantified case.

```raku
my $s = "z";
say "(xy)" ~~ m/ $s? /;   # now: ｢｣ (matches raku)
my $p = "%";
say "ab" ~~ m/ $p? b /;   # now: ｢b｣ (matches raku)
my $t = "abc";
say "abcabc" ~~ m/ $t? /; # now: ｢abc｣ — the whole value as one atom
```

New test: `t/regex-quantified-scalar-interpolation.t`.
