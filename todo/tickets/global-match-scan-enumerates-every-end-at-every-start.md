# The `:g` / `subst` scan enumerates every end at every start position

Split out of ADR-0073's measurement sweep (2026-09-07). Slice 1 made the *atom*
boundary demand-driven, which fixed a block's count everywhere the walk drives
the match — but `regex_match_all_*` (`src/runtime/regex/regex_match_find.rs`)
does not drive a walk, it brute-forces the subject:

```rust
starts.extend(0..=orig_chars.len());
for start in starts {
    let ends = self.regex_match_ends_from_caps_in_pkg(&parsed, orig_chars, start, &pkg);
    for (end, mut caps) in ends { ... }
}
```

Every start position runs the whole pattern, and (outside `canonical_only`)
collects every end at that position. raku instead finds one match, commits to
it, and resumes the scan **after** it.

## Measured against `raku` (2026-09-07, after ADR-0073 Slice 1+3)

```raku
my $c = 0; my @m = "aa bb" ~~ m:g/ ( \w* { $c++ } ) /;  say $c;
# raku: 4    mutsu: 12

my $d = 0; my $s = "aaa".subst(/ ( \w* { $d++ } ) /, 'X'); say $d;
# raku: 1    mutsu: 2
```

Both rows are pinned as `todo` in `t/regex-lazy-candidate-enumeration.t` (A14,
A15). Before ADR-0073 they were 12 and 5, so Slice 1 already halved the `subst`
row; what is left is the scan itself.

The `subst` row is the interesting one: `canonical_only` already breaks after
the first end at a position, yet the block still runs twice — so the pattern is
being matched at more than one start position (or twice at one) before the
replacement is built. Find out which before designing anything; the two answers
call for different fixes.

## Why it is a ticket and not a deep item

The change is local to one function and its `canonical_only` twin: walk start
positions in order, take the first match (`first_only`), record it, and continue
the scan from its end (from `start + 1` for a zero-width match, which is what
raku does). The risk is that several callers lean on "every end at every start"
— `.match(:g)`, `:nth`, `:x`, `comb`, `split`, and the `Match` list a bare `:g`
returns — so the first step is to enumerate the callers of
`regex_match_all_*` and check which of them actually want overlapping ends.
