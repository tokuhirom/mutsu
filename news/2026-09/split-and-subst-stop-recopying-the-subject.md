# `split` and global `subst` stop re-copying the subject on every match

`.split(/rx/)`, `.subst(rx, :g)` and `s:g///` were quadratic in subject length.
Doubling the input roughly quadrupled the time, so the cost was invisible on the
short strings every regex benchmark used and ruinous on anything real:

| subject | `.split(/\s+/)` | rakudo | `.subst(/\d+/,:g)` | rakudo |
| --- | --- | --- | --- | --- |
| 5 KB | 52 ms | 590 ms | 12 ms | 228 ms |
| 20 KB | 545 ms | 241 ms | 29 ms | 227 ms |
| 80 KB | 11877 ms | 257 ms | 260 ms | 292 ms |
| 640 KB | >60 s (timeout) | 498 ms | 23174 ms | 786 ms |

`.comb` over the same subject was flat (18 ms at 80 KB), which is what made the
shape legible: the scan was never the problem.

## What it was

Both operations scan one unchanging subject once per match, and both rebuilt the
whole subject's character axis on every one of those calls.

`.split` called `regex_match_with_captures_from` per separator, which opens with
`MatchTarget::new(text)` — an `Arc<String>` copy plus an `Arc<[char]>`, about 5
bytes per character. `.subst(:g)` and `s:g///` called
`regex_find_first_from_with_all_captures` per match, which did the same thing by
hand (`text.chars().collect()`). On a 64 KB subject with 1130 matches that is
~360 MB of copying, and a callgrind run of an ordinary 32 KB substitution put
**66% of all executed instructions inside that one `collect`**, with another 12%
in the `memcpy` it drives.

The measurement also corrected the root cause this was first filed with: the
suspect was the per-match `MatchTarget` in `subst_match_var` (the `$/` list), but
`MatchTarget::new` runs exactly twice in that profile. The `.subst` method never
reaches `dispatch_subst` at all — it goes through the VM's native fast path
(`native_subst_regex`), and the per-match cost was in the scan step it calls, not
in the `$/` construction. Profiling before patching was the whole difference
between a fix and a plausible-looking no-op.

## What it is now

Both scan entry points take a subject that is already materialized:
`regex_match_with_captures_from_target` and
`regex_find_first_from_with_all_captures_in`. The four callers that loop over one
subject — `native_subst_regex` (`.subst`), `subst_collect_matches` (`s///`,
`S///`), `split_by_regex` / `split_by_regex_list`, and the non-PCRE2
`regex_find_all_p5_with_captures` — build one `MatchTarget` outside the loop and
share it. The `&str`-taking wrappers had no remaining callers and are gone, so
the cheap-looking signature that invited the bug no longer exists.

`SplitMatch::orig` went with them: a `String` copy of the whole subject stored on
every separator, which nothing had read since separator `Match` objects started
carrying their own target. Five of its seven initializers were already
`String::new()`.

Result, same box, same rakudo 2026.07:

| subject | `.split(/\s+/)` | `.subst(/\d+/,:g)` |
| --- | --- | --- |
| 5 KB | 52 ms -> 14 ms | 12 ms -> 11 ms |
| 20 KB | 545 ms -> 22 ms | 29 ms -> 14 ms |
| 80 KB | 11877 ms -> 52 ms (**228x**) | 260 ms -> 24 ms (11x) |
| 640 KB | timeout -> 393 ms (0.49x rakudo) | 23174 ms -> 128 ms (**181x**) |

Both are linear now, and land where `.comb` already was: 0.05-0.20x rakudo across
the whole range instead of 46x at 80 KB.

## Keeping it

`benchmarks/bench-regex-split-subst.raku` (added days earlier, which is how this
was found) was sized so the old quadratic cost showed as a few hundred
milliseconds; at linear cost that collapsed to 30 ms, so it is re-sized to 256 KB
through `split` and 512 KB through `subst` — sizes the old code could not have
finished inside the bench timeout.

A timing assertion would be flaky, so the shape of the fix is pinned instead: a
new `MUTSU_VM_STATS` counter, `match_targets`, counts whole-subject
`MatchTarget` constructions, and `tests/regex_subject_materialized_once.rs` runs
1500 matches across the three operations and asserts the count stays in single
digits (it is 3 — one per call). Closes
[#8247](https://github.com/tokuhirom/mutsu/issues/8247).
