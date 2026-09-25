# Multi-separator `.split` is linear again

`.split([sep1, sep2, ...])` and its regex-list form `.split([/rx/, str])` searched
every separator again from the start of each piece. A separator that was rare or
absent was therefore scanned to the end of the string once per piece, making one
call O(s·n·k) — effectively quadratic. `("a," x 40000).split([",", ";"])` took
about 5.5 s against Rakudo's 0.07 s ([#9145](https://github.com/tokuhirom/mutsu/issues/9145)).

Both forms now keep each separator's next match and search again only once the
cursor has moved past its start — a k-way merge of the per-separator find
streams. A cached match is still the first one at or after the current cursor,
so the earliest-start / longest-on-tie choice is unchanged. For the regex-list
form this relies on every search running against the one shared full-text
`MatchTarget`, so a match's extent does not depend on where the search began;
the winning match's captures are moved out of the cache rather than cloned.

The duplicate `split_by_strings_static` in `src/runtime/builtins_string.rs` was
deleted in favour of the one `split_by_strings` in `src/builtins/split.rs`.

`scripts/str-complexity-check.sh split` (release, 4-core container) now reports
a t(2N)/t(N) ratio of 1.99 for the string list (was 3.95) and 1.83-2.16 for the
new regex-list case at N = 20k-80k (was ~3.6); the regex-list call at N = 10k
went from 0.36 s to 0.02 s.

Pinned by `t/types/string/split-multi-separator.t`.
