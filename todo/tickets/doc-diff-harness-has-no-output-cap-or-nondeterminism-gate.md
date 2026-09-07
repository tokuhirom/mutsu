# The doc-diff harness has no output cap and no oracle-nondeterminism gate

Two independent defects in `scripts/doc-diff-harness.raku` that together
misdirect the whole doc-diff campaign. Both were measured on the 2026-09-07b
full-corpus sweep at `dccfd1737`.

## 1. `raku-drift-from-doc` is a priority signal derived from the wrong fact

The bucketing at `scripts/doc-diff-harness.raku:72-93` is:

```
if mutsu output == raku output   -> match
elsif mutsu exited non-zero      -> mutsu-error
else                             -> if raku output != the doc's `# OUTPUT:` -> raku-drift-from-doc
                                    else                                     -> output-mismatch
```

The `raku-drift` branch is **only reachable once mutsu already differs from
raku**. But `docs/doc-diff-backlog.md` describes that bucket as "version skew,
not mutsu bugs — lowest priority" and the harness's own comment says "mutsu may
well match the doc". Measured composition of the 114 drift blocks:

| verdict | count | share |
|---|---|---|
| **REAL** — deterministic mutsu-vs-raku divergence | **67** | 59% |
| NONDET-only — the whole diff is an unreproducible token | 33 | 29% |
| ENV-only | 9 | 8% |
| MUTSU-MATCHES-DOC — what the bucket name describes | 5 | 4% |

So the doc's `# OUTPUT:` annotation is being used as a **priority** signal when
it is only a **provenance** signal. 67 confirmed real divergences sit in the
campaign's lowest-priority bucket, against 5 that the name actually fits.

**Fix:** fold the bucket into `output-mismatch` and keep the doc comparison as
an *annotation* on the finding ("mutsu matches the doc here"), not as a bucket.

## 2. No gate on the oracle's own reproducibility

37% of the drift blocks (42 of 114) are there solely because the doc froze a
token that **raku itself cannot reproduce twice**: unordered-container
iteration order (27 blocks — `Set`/`Bag`/`Mix`/`*Hash`/`Map`/`Hash.kv`/enum
`.keys`), object addresses and `WHICH` ids (13 — `Block|94212856419136`,
`@Array_75093712`, …), a thread id, and one explicitly racy example. Measured:

```
$ for i in 1..5; raku -e 'say (bag <a b c>).kv.join(",")'
c,1,b,1,a,1
a,1,c,1,b,1
c,1,b,1,a,1
a,1,c,1,b,1
b,1,c,1,a,1
```

against a deterministic control that is byte-identical 5/5. **Nine real mutsu
bugs were hiding under this class**, including
`code-object-renders-as-nothing-inside-a-list`, because a doc line containing an
address pushes every such example into the low-priority bucket on every run,
forever.

**Fix (one line of policy, no pattern list):** run the oracle **twice** and drop
the block when raku's own output is not reproducible. That removes all 42 at the
source and needs no `# OUTPUT:` annotation to exist. The existing
`nondeterministic()` filter (line 233) cannot do this job — it already skips
`.WHERE` but not `.WHICH`, `now`/`time` but not `$*DISTRO`/`$*VM`/`$*RAKU`/
`$*EXECUTABLE`/`dir`/`indir`, and enumerating every unordered-container spelling
is exactly the brittle path to avoid.

## 3. No cap on captured output

A single doc example can produce a multi-megabyte report, and the refresh recipe
in `docs/doc-diff-sweep/README.md` commits it:

- `Type/IO/Path.rakudoc:509` is a `sub MAIN` that recursively `.dir`-walks the
  working directory. It enumerated the whole repo including `.git/` and `tmp/`
  and produced a **131 492-line, 8.4 MB** report.
- `Language/ipc.rakudoc:34` shells out and captured the full git log — 1.6 MB.

The 2026-09-07b sweep was committed only after truncating captured-output
sections by hand (11 MB → 412 KB). Cap each captured section in the harness
(≈40 lines) with an explicit truncation marker so the recipe is safe as written.

## 4. The sweep writes files into the repository root

Doc examples that `spurt`/`open` a relative path run with the repo root as cwd,
so the 2026-09-07b sweep left empty `bar` and `foo.txt` files behind. Run each
block in a scratch directory (or at least sweep for strays afterwards) so a
sweep cannot dirty the working tree.

## Acceptance

- `raku-drift-from-doc` no longer exists as a *priority* bucket; the doc
  comparison survives as an annotation.
- The oracle runs twice and non-reproducible blocks are dropped, with a counter
  reported alongside `match`/`mismatch`/`crash` so the noise floor is visible.
- No committed report section exceeds the cap.
- Re-sweep and confirm the 27 unordered-container blocks and the 13
  address blocks are gone, and that the 9 real bugs they hid now surface as
  ordinary `output-mismatch` findings.
- No stray files are left in the repository root after a sweep.
- Update `docs/doc-diff-backlog.md`'s "Always re-verify" paragraph, which
  currently tells the reader that drift findings are "not mutsu bugs".
