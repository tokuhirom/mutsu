# The doc-diff harness gates on the oracle's own reproducibility

Four defects in `scripts/doc-diff-harness.raku` that between them misdirected the
whole doc-diff campaign. All four were measured on the 2026-09-07b full-corpus
sweep.

## `raku-drift-from-doc` was a priority signal derived from a provenance fact

The harness cross-checked every mutsu-vs-raku divergence against the doc block's
own `# OUTPUT:` annotation, and when raku no longer matched the doc it filed the
finding in a separate bucket documented as "version skew, not mutsu bugs —
lowest priority".

But that branch is only reachable *once mutsu already differs from raku*.
Whether the doc is stale says nothing about whether mutsu is wrong. Measured
composition of the 114 blocks in that bucket: 67 real deterministic mutsu
divergences, 33 whose entire diff was an unreproducible token, 9 environment-only,
and 5 — 4% — that the bucket's name actually described.

The bucket is gone. Every divergence is one `output-mismatch` finding, and the
doc comparison survives as an annotation on it ("mutsu matches the doc's own
`# OUTPUT:` here; raku does not"), which is what the fact actually supports.

## The oracle was never checked against itself

37% of those blocks (42 of 114) were there only because the doc froze a token
that **raku itself cannot reproduce twice**: unordered-container iteration order
(27 blocks — `Set`/`Bag`/`Mix`/`*Hash`/`Map`/`Hash.kv`/enum `.keys`), object
addresses and `WHICH` ids (13), a thread id, and one explicitly racy example.
Such a block diverges on the unreproducible token alone, on every run, forever —
so it lands in the low-priority bucket permanently. Nine real mutsu bugs hid
there, [#7587](https://github.com/tokuhirom/mutsu/issues/7587) among them.

The harness now runs the **oracle twice** and drops the block unless raku agrees
with itself. That is the whole policy: one rule, no pattern list. It deliberately
replaces growing the existing `nondeterministic()` heuristic, which already
skipped `.WHERE` but not `.WHICH`, `now`/`time` but not `$*DISTRO`/`$*VM`/`dir` —
enumerating every unordered-container spelling is exactly the brittle path to
avoid. The count is reported as `skipped (oracle not reproducible)` beside
match/mismatch/crash so the noise floor is visible rather than silent.

## No cap on captured output

A single example could bury a sweep. `Type/IO/Path.rakudoc:509` is a `sub MAIN`
that recursively `.dir`-walks its working directory; it enumerated the whole repo
including `.git/` and produced a **131 492-line, 8.4 MB** report.
`Language/ipc.rakudoc:34` shells out and captured a 1.6 MB git log. The
2026-09-07b sweep could only be committed after truncating 11 MB down to 412 KB
**by hand**, which is what the refresh recipe told the reader to do.

Each captured section is now cut to 40 lines with an explicit
`... [truncated by doc-diff-harness: N more line(s) of M]` marker, so a truncated
section is never mistaken for a whole output and the recipe is safe as written.

## Sweeps wrote into the repository root

Doc examples that `spurt`/`open` a relative path ran with the repo root as cwd,
so the 2026-09-07b sweep left empty `bar` and `foo.txt` behind. Every block now
runs inside a throwaway scratch directory, recreated empty before each run — also
required by the double-oracle gate, since a block that merely *appends* to a file
would otherwise look non-reproducible. (Making the runs chdir meant absolutising
the `--mutsu` path, which is relative by default.)

That change alone shrank the `Type/IO/Path.rakudoc` report from 8.4 MB to 427
bytes: the recursive `.dir` walk now finds an empty directory instead of the repo.

## Verified

On `Type/{Bag,Set,Code}.rakudoc`, 4 blocks are dropped by the new gate — the
`Set`/`Bag` iteration-order class the ticket named — and the remaining 24
comparisons resolve to 21 match / 2 mismatch / 1 crash instead of being scattered
into a low-priority bucket. A fixture with a 61-line divergent output truncates
both sections at 40 lines with the marker. A sweep leaves the working tree clean.

`docs/doc-diff-backlog.md`, `docs/qa-doc-diff-harness.md` and
`docs/doc-diff-sweep/README.md` are updated: the backlog's "Always re-verify"
guidance no longer tells the reader that drift findings are "not mutsu bugs", and
the README no longer instructs a manual truncation pass. The dated 2026-09-07b
corpus snapshots keep their original numbers as history, marked as recording the
retired bucket. `scripts/doc-diff-sweep.sh` aggregates the new `nondet` key in
place of `raku-drift`.

Closes #7590.
