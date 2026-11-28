# A regex that names a scalar no longer re-scans its text on every match

`$line ~~ / $needle \d+ /` reads `$needle` where the cursor reaches it
(ADR-12917, #14510), so the pattern's parse does not depend on the variable's
value. The match entry points still redid the text work on every match:

- `with_match_time_scalars` re-scanned the pattern for its scalar names twice
  per match (`regex_match_with_captures`, then `parse_regex`), and the second
  call rebuilt a declaration block the first had already written. The scan is
  now memoized by text, and an already-marked pattern is returned as it is.
- `parse_regex`'s dynamic branch re-ran the interpolation pre-pass (a
  `Vec<char>` of the pattern and a fresh output `String`) and probed its cache
  by the result. The pre-pass now reports whether it read any state at all; a
  pattern for which it read nothing is memoized by its text
  (`REGEX_PURE_DYNAMIC_PARSE_CACHE`), like a static pattern. A pattern that
  splices a value, an array, a qq atom or an enclosing pattern's `:my` name is
  not.
- `parse_anchored_single_subrule` built a whitespace-free copy of every
  pattern before checking for its `^ <name> $` shape.

`bench-regex-match`'s line 7 shape, `MUTSU_JIT=off`, callgrind slope: **48.7k ->
36.7k instructions per match (-25%)** and **57 -> 32 allocations per match
(-44%)**. Raku++ v5.9.0 is still about 3x faster on that line; most of what
remains is publishing the variable into the env before each match and the
per-match regex closure scope (#19296).
