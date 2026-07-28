# Match-object construction shares `.orig` via `Arc` instead of re-cloning the whole document per leaf capture

`Value::make_match_object_full_q` (the Match-object builder behind grammar
`.parse()`, `~~`, substitution, and `split`) builds one `Match` `Instance` per
node in a match tree — including a fresh one for every element of a
quantified capture like `<str=space>*`, so a document with a long run of
individually-matched characters (e.g. a quoted YAML scalar's embedded spaces)
can have thousands of leaf `Match` objects for one `.parse()`.

Two of its internal helpers, `make_capture_match` and `make_subcap_match`,
took `orig: Option<&str>` and, **per leaf capture**, both re-collected the
entire original string into a fresh `Vec<char>` (to search for the capture's
position) and re-cloned it into a fresh `String` (for the `.orig` attribute).
For a leaf-heavy match tree this made one `.parse()` cost O(captures ×
document length) in redundant allocation and copying, independent of the
document's actual content.

Both helpers now take a small `OrigCtx` pair — `&Arc<String>` (for `.orig`,
via `Value::str_arc`, an O(1) refcount bump instead of a fresh allocation) and
`&[char]` (a borrowed slice of a `Vec<char>` built once) — computed a single
time per `.parse()`/match at the top of `make_match_object_full_q`, rather
than re-derived at every leaf.

## A methodology note, since this landed without a clean local measurement

While investigating this fix's actual wall-clock effect, this session found
eight `sh` processes on the dev box running a busy-loop
(`while :; do :; done`) continuously since 2026-07-25 — a `sleep 12; kill`
script whose backgrounded loops never actually died, silently consuming ~6
cores for 3 days. On this box's hybrid P-core/E-core chip, that contention
made every local A/B in this session (including round 2's, in the PR just
before this one) unreliable: a "before" and "after" binary measured
identically once the spinners were killed and the comparison re-run pinned to
one core. See `todo/tickets/yaml-parse-throughput.md` for the full account
and the lesson (`ps --sort=-pcpu` before trusting any local perf number here).

This fix is still landed on its own merits — it is a correct, verified
reduction in real, measurable-in-principle redundant work (confirmed by
reading the code, not by a wall-clock delta), and the full `t/` suite (2553
files, 24412 tests) plus `Match.orig`/position-tracking-specific tests all
stay green. Its actual size of effect, if any, on real documents will show up
in `bench-history.tsv` on `bench-data` via the newly-added
`benchmarks/bench-yaml-parse.raku`, not in this write-up.
