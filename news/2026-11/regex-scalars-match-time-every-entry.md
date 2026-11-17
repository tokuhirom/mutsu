# Every match entry point reads a regex `$var` at match time

A plain `$var` in a regex is a match-time atom (ADR-12917, direction 2): it is
read when the cursor reaches it, so a code block earlier in the pattern can
change what it matches. Until now only the smartmatch entry point declared a
pattern's scalars as match-time lexicals. `s///`, `.match`, `.comb`,
`.subst`, `m:g`, `.split` and the value-aware `:i`/`:m` fallback still
spliced the value into the pattern text before the match started (#14510):

```raku
my $x = "a"; my $s = "xbab";
$s ~~ s/x { $x = "b" } $x/Y/;
say $s;   # Yab (was xbab)
```

These entry points now parse through `parse_regex_for_match`, which applies
the same `with_match_time_scalars` declaration as the smartmatch path.
