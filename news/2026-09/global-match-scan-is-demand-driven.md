# The `:g` / `subst` scan runs the pattern once per committed match

Retires the last two `todo` rows of `t/regex-lazy-candidate-enumeration.t`
(A14, A15), left open by ADR-0073 Slice 1.

```raku
my $c = 0; my @m = "aa bb" ~~ m:g/ ( \w* { $c++ } ) /;  say $c;
# raku: 4    mutsu: 12   (before)

my $d = 0; my $s = "aaa".subst(/ ( \w* { $d++ } ) /, 'X'); say $d;
# raku: 1    mutsu: 2    (before)
```

raku finds one match, commits to it and resumes the scan **after** it. mutsu
brute-forced the subject: every start position ran the whole pattern, and at
each one the walk explored the whole backtracking tree. The *reported* matches
were already right — `select_non_overlapping_matches` threw the extras away —
but a `{ ... }` block inside the pattern had already run for every discarded
candidate.

## Three layers, three fixes

**The subst scan ran past its own answer.** `native_subst_regex` collected
*every* match into a `Vec` and then sliced `&matches[..1]` for a non-global
`.subst`. It now breaks out of the loop after the first match when `!global`.
This was the whole of the A15 row.

**The `:g` scan ran the pattern at covered positions.** `regex_match_non_overlapping`
replaces `regex_match_canonical_per_start` + `select_non_overlapping_matches`:
it skips a start that a previously accepted match already covers, so the
post-filter has nothing left to discard. A zero-width match does not advance the
barrier, so a `\w*` matching empty right after a previous match is still
reported — exactly the `from >= last_end` test the post-filter applied, and the
reason `"aa bb" ~~ m:g/(\w*)/` still answers `("aa", "", "bb", "")`.

**The walk explored ends the caller discards.** With `canonical_only` the impl
keeps only the first end at a start, and the DFS finds it first — so it now
calls the existing `regex_match_end_from_caps_in_pkg` (which passes
`first_only: true` and stops the walk) instead of `regex_match_ends_from_caps_in_pkg`.
This was the residual factor of two that remained after the first two fixes.

## A correctness fix came with it

The smartmatch `:g` arm still used `regex_match_all_with_captures` — every end
at every start, then longest-per-start — which is precisely the shape
`regex_match_canonical_per_start` had been introduced to replace on the
`.match(:g)` side. So a top-level frugal quantifier was forced to its greedy
length there and nowhere else:

```raku
say ("aXbXcX" ~~ m:g/.*?X/).map(*.Str).raku;
# raku:  ("aX", "bX", "cX").Seq
# mutsu: ("aXbXcX",).Seq          (before)
say "aXbXcX".match(/.*?X/, :g).map(*.Str).raku;   # already correct in both
```

Routing the three non-overlapping smartmatch arms (`:g`, `:nth`, and `:x`
without `:g`) through the same scan fixes it. The `:overlap` and `:exhaustive`
arms genuinely need every end at every start and keep using
`regex_match_all_with_captures`; so does the PCRE2 (`:P5`) branch.

## Measured against `raku`, all matching

The two repro counts; the block count for `\w*` and `\w+` under `:g` over
`"a"`, `"aa"` and `"aa bb"`, and for a single (non-`:g`) match; the frugal
quantifier through both `~~ m:g//` and `.match(:g)`; and the results of
`.match(:g)`, `.subst(:g)`, `comb`, `split`, `:nth` and `:x`, which were correct
before and are unchanged.

## Testing

`t/regex-lazy-candidate-enumeration.t` A14/A15 lose their `todo` and pass; the
file passes unchanged under rakudo. The 188 `t/regex*.t` / `t/subst*.t` /
`t/match*.t` / `t/comb*.t` / `t/split*.t` files (2008 assertions) pass.
