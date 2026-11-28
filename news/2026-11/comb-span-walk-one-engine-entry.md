# `.comb(rx)` walks its subject in one regex-engine entry

`regex_find_all_limited`, the span walk behind `.comb(rx)`, entered the regex
engine once per candidate start. Each entry repeated the same setup: the
guards, the program lookup, a scratch state taken from and returned to the
thread-local pool, and a new scan iterator for every match. Each match also
snapshotted its captures into a `RegexCaptures` that the walk dropped at once,
since it wants only the span.

The unanchored scan loop that `rx_scan_first` already ran in one entry is now
shared (`rx_scan_loop`). A new `rx_scan_all_spans` uses it to walk the whole
subject: the scan iterator is advanced past each match instead of rebuilt, and
a `Goal::FirstEnd` returns the end without the snapshot. Code blocks stay inert,
as they were on the per-start path. Anchored and `:ignoremark` patterns keep
the old loop. Mapping an ASCII subject's char spans to bytes also stopped
walking the text.

`comb` line of `bench-regex-global.raku` (480 pieces per operation;
`profiling` build, `MUTSU_JIT=off`, callgrind): **1.14M -> 0.82M instructions
per operation (-28%)**, 5.9 -> 3.7 ms locally for 45 operations. With #19335
that line went 6.5 -> 3.7 ms. Raku++ v5.9.0 takes 1.7 ms (#19296).
