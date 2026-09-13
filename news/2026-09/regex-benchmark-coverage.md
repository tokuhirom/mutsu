# The benchmark suite learns to measure regexes

Until now the `benchmarks/` suite had essentially no regex coverage. Of its 26
files, exactly one touched the engine directly: `bench-string.raku`, whose third
section runs `"hello world 42 foo" ~~ /\d+/` 5000 times alongside 10000 string
concatenations and 1000 splits, so a regex change moves that row by a fraction
of its own noise. The grammar files (`bench-grammar-parse`,
`bench-grammar-parse-deep`, `bench-yaml-parse`) do exercise the matcher, but
through grammar machinery and at sizes of a few hundred bytes -- all three run in
16-70 ms on CI, a few times the 8 ms startup.

That is a poor fit for where the work actually goes. Substitution, global
matching, `split` on a pattern, captures and `Match` accessors, look-around, word
boundaries, backtracking, interpolated patterns and ratchet semantics had no
series at all, and #7576 -- twenty-two rounds of regex and grammar performance
work -- had been measured the whole way on a 6-row YAML document plus ad-hoc
local runs.

Eight new files close that:

| file | what it isolates |
| --- | --- |
| `bench-regex-match` | the matcher with no Match built: classes, quantifiers, alternation/LTM, `:i`, anchors, a failing scan, interpolated patterns |
| `bench-regex-capture` | what a *successful* match has to hand back -- named and positional captures, quantified and nested groups, `$/.from`/`.to`, and a capturing match that fails |
| `bench-regex-global` | resume-from-last-end scanning: `.match(:g)`, `.comb`, `.subst(:g)`, `s:g///`, and a closure replacement that reads `$0` per hit |
| `bench-regex-assertion` | zero-width assertions and backtracking: `<?before>`/`<!before>`, `<?after>`/`<!after>`, `<<`/`>>`, give-back, `:r`, separated quantifiers |
| `bench-regex-long-subject` | per-position reject cost: one 128 KB subject, nine single-call scans, eight of them failing |
| `bench-regex-split-subst` | the paths that assemble a result from every occurrence, which are superlinear today |
| `bench-grammar-parse-big` | the same JSON-like grammar as the small files, on a 10 KB document, so growth rate is visible |
| `bench-yaml-parse-big` | YAMLish on the 60-row document #7576's own numbers are quoted at |

Every file prints a checksum and produces byte-identical output under rakudo
2026.07, so each is a correctness check as well as a measurement, and none uses
randomness or wall-clock time. Each costs 0.15-0.33 s locally, which keeps the
added bench-CI time (wall-clock pass x2 configurations, plus the callgrind
instruction-count pass x2) to a few minutes.

## Two findings the new files were written around

Writing them turned up two things the old suite could not see.

**`.split(rx)` and `.subst(rx, :g)` are quadratic in subject length**
([#8247](https://github.com/tokuhirom/mutsu/issues/8247)). Doubling the subject
roughly quadruples the time: an 80 KB `split(/\s+/)` takes 11.9 s against
rakudo's 0.26 s (46x), and a 640 KB global substitution 23.2 s against 0.79 s
(29x). `.comb` over the same subject is flat (18 ms at 80 KB), so the scan is
fine; the per-match result assembly is not. Each match builds its own
`MatchTarget`, and that allocates a fresh `Arc<String>` *plus* an `Arc<[char]>`
of the whole subject -- about 5 bytes per character, per match.

**Scanning is linear but its per-position constant is ~8x rakudo's**
([#8248](https://github.com/tokuhirom/mutsu/issues/8248)). A failing alternation
scan is 0.21x rakudo at 40 KB, 0.50x at 160 KB and 1.25x at 640 KB: the apparent
win at small sizes is rakudo's ~200 ms startup, and it is spent by about half a
megabyte. Succeeding scans that stop early stay at 0.05x at every size, so the
cost is specifically on the reject path -- the work to advance one position and
fail, which is also the inner loop of every grammar parse.

Both are perf issues rather than wrong answers, and both now have a benchmark
that will show the fix.
