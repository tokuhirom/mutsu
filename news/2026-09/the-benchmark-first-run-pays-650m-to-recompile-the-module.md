# The first run after a rebuild pays 650M instructions to recompile the module

A correction, and the measurement hazard behind it.

`news/2026-09/a-rejected-parse-alternative-stops-allocating-its-message.md`
(#8859) reported that the first `from-json` call costs a fixed **668M
instructions** regardless of document size, and that this is 27% of #8830's
100-record reproduction. **That is wrong.** It was a warm run compared against
a cold one.

## What is actually happening

Rebuilding (or merely `touch`ing) `target/release/mutsu` invalidates the
module precompilation cache, and the **next** run pays to recompile
`JSON::Fast`. Every run after that does not.

Same binary, same input, `modules/JSON-Fast/lib/.precomp` file listing
identical before and after, nothing touched between runs:

| | Ir |
| --- | ---: |
| run 1 | 2,542,308,493 |
| run 2 | 1,892,071,969 |
| run 3 | 1,892,265,123 |

**650M — 34% of the run — is compiling the module, and it is paid once.** The
same effect is visible natively: the benchmark is ~250-290ms warm, and the
first run after `touch target/release/mutsu` is ~70-100ms slower, which is
what 650M instructions cost at the ~7.6 GIPS this benchmark sustains.

The #8859 claim came from comparing `tmp/load_only.raku` (run many times
already, so warm) against a freshly written `tmp/load_plus_tiny.raku` (cold).
The 668M was the cold-start compile, not a cost of calling `from-json`.
Measured properly — release binary, both warm — adding `from-json('[1]')` to
the load-only script costs **24.3M instructions** (65,863,549 → 90,172,523),
about 4ms. That is the real number, and it is unremarkable.

## What this does and does not invalidate

**The four slices' A/B deltas stand.** Each was measured as one run per
binary, and a fresh build means a cold run — so both sides of every comparison
were cold, and the comparison is like-for-like:

| slice | before | after | |
| --- | ---: | ---: | ---: |
| [#8843](https://github.com/tokuhirom/mutsu/pull/8843) | 2,975,774,664 | 2,643,873,087 | -11.2% |
| [#8851](https://github.com/tokuhirom/mutsu/pull/8851) | 2,643,952,646 | 2,579,086,749 | -2.45% |
| [#8854](https://github.com/tokuhirom/mutsu/pull/8854) | 2,584,028,104 | 2,547,894,577 | -1.40% |
| [#8859](https://github.com/tokuhirom/mutsu/pull/8859) | 2,548,759,824 | 2,471,953,037 | -3.01% |

**Their "share of the program" figures do not stand**, because the denominator
in every one of them includes the 650M compile pass that a steady-state run
does not do. Real steady-state work on this input is ~1.89G, not ~2.5G.

The consequence is not uniform, and it matters most for **#8859**: what that
slice removed was parser allocation, and the parser is *in* the compile pass.
So its saving lands almost entirely in the once-per-invalidation cost rather
than in parsing the document — which is exactly why its 727-record wall clock
did not move. It remains a real improvement to **compilation**, which every
first run, every `-e` script, every un-precompiled module and all 1437 files
of `make test` pay; it is not an improvement to parsing a large JSON document,
and the #8859 entry should be read with that correction.

## How to measure this benchmark

- **Discard the first run after any build.** It is cold by construction, and
  it is 34% high.
- Quote a **second or later** run, or state explicitly that the figure is cold
  and compare only against another cold one.
- A warm/cold mismatch is worth 650M instructions, which is larger than every
  individual win this issue's slices have produced. It is the single easiest
  way to measure a change that does not exist — or to miss one that does.
- `--profile profiling` is a separate build from `--release`, so switching
  between them invalidates the cache too. The numbers in this entry's first
  table were taken with the release binary.
