# Quantified scalar interpolation in regex never matches (`$s?` / `$s+`)

A quantifier attached to an interpolated scalar fails to match:

```raku
my $s = "z";
say "(xy)" ~~ m/ $s? /;   # raku: ｢｣ (empty match), mutsu: False
my $p = "%";
say "ab" ~~ m/ $p? b /;   # raku: ｢b｣, mutsu: False
```

Pre-existing (not introduced by the 2026-08-13 blanket-escape fix — the
same failure reproduces with alphanumeric values that were never escaped).

## Root cause direction

`interpolate_regex_scalars` (src/runtime/regex_parse_modifier.rs) splices
the scalar's escaped VALUE into the pattern text, wrapped in
`NON_DECLARATIVE_INTERP_MARK` sentinels, and leaves the following `?`/`+`/
`*` in place. The structural parser then fails to attach the quantifier to
the mark-wrapped span (and for a multi-char value it could at best
quantify the LAST char, while raku quantifies the whole interpolated value
as ONE atom: `my $s = "abc"; m/ $s? /` optionally matches the whole
"abc").

The text-splicing design cannot express "this spliced span is one atom".
Fix direction: lower a quantified `$var` to a real token (like the
existing match-time `VarInterp` atom, which already exists for `:my`
regex-locals) instead of splicing text, or make the parser treat a
MARK...MARK span as a single group atom so a trailing quantifier binds to
all of it.

Found while pinning the tilde-escape fix (t/regex-scalar-interp-
metachar-literal.t); Text::CSV does not need it, so it was deferred.
