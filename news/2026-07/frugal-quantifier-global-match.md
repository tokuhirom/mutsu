# Frugal quantifiers match minimally under `:g`

A top-level frugal (lazy) quantifier — `.*?`, `.+?`, `** N..*?` — now matches
minimally under global matching (`.match(..., :g)`), as it always did for a
single match:

```raku
"aXbXcX".match(/ .*? 'X' /, :g).map(*.Str);   # («aX» «bX» «cX») — was one match «aXbXcX»
"aXbXcX".match(/ $<c>=(.*?) 'X' /, :g)».<c>;   # (a b c)          — was (aXbXc)
```

mutsu's global-match path collected *every* possible match end at each start
position and then kept the longest per start (via `select_non_overlapping_matches`),
which is right for a greedy quantifier but forced a frugal top-level quantifier to
its greedy length. Single (non-`:g`) matches were already correct because the
single-match engine returns match ends in DFS-completion order, highest-priority
(canonical greedy/frugal) first, and takes the first.

The plain `:g` path now uses the same canonical choice: a new
`regex_match_canonical_per_start` keeps only the highest-priority end at each
start (the one the single-match engine would pick), which
`select_non_overlapping_matches` then tiles into non-overlapping matches. The
`:overlap` and `:exhaustive` paths still enumerate every end, and `s:g///` was
already correct, so only plain `.match(..., :g)` changed.

This unblocks the `Text::CodeProcessing` distribution's markdown/org/pod code-chunk
extraction (T-057), whose search regex uses a frugal `.*?` code body between the
opening and closing fences under `:g`. Pin: `t/frugal-quantifier-global-match.t`.
