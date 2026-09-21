---
name: clippy-clone-sweep
description: Run cargo clippy with clippy::redundant_clone (plus nursery/pedantic as a noisy superset) to find genuinely wasted `.clone()` calls, verify a sample, then either fix a small batch directly or file the rest as todo:ticket issues grouped by module. Use when asked for a "clone sweep" / "clippy nursery" pass, or periodically as hygiene — this is not a bug hunt, it is a mechanical quality pass.
metadata:
  short-description: Periodic clippy::redundant_clone sweep -> issues or a direct fix
---

# clippy redundant-clone sweep

There is a real theory behind this: `clippy::redundant_clone` finds `.clone()` calls whose
result is provably never used again, which is a genuine wasted allocation every time — not a
style nit. `clippy::nursery` / `clippy::pedantic` as a whole do **not** — they bury it under
tens of thousands of unrelated warnings. This skill is how to get the signal without drowning in
the noise, and what to do with what you find.

## 1. Run it — focused, not the whole nursery/pedantic superset

```sh
mkdir -p tmp
cargo clippy --all-targets --message-format=json \
  -- -W clippy::redundant_clone \
  > tmp/clippy-clone-sweep.json 2> tmp/clippy-clone-sweep.log
```

This alone is the useful command for routine sweeps. Warm-incremental it is a normal `clippy`
run (well under a minute); a cold one pays a full `cargo build`-equivalent compile, so start it
`run_in_background: true` and wait for the notification per the 30-minute-polling-floor rule in
CLAUDE.md — do not tail the log.

**Do not default to adding `-W clippy::nursery -W clippy::pedantic -W clippy::str_to_string
-W clippy::string_to_string`.** A run with all of those on this codebase (2026-09-20 baseline)
produced 62,566 warning messages; `clippy::str_to_string` alone was 15,581 of them and is mostly
not about clones at all (it just flags every `&str -> String` conversion, most of which are
correct and necessary). If you do want the wider net — e.g. investigating whether `implicit_clone`
turns up something `redundant_clone` misses — add lints one at a time and re-check the noise
ratio; don't run the whole superset and try to read the result by eye.

`clippy::implicit_clone` (a pedantic lint, ~696 hits at the same baseline) is a reasonable second
pass once `redundant_clone` is under control: it flags an indirect clone written as `.to_vec()`/
`.to_owned()` where a plain `.clone()` (or no clone at all) would do. Same triage process below
applies; just swap the lint name in the JSON filter.

## 2. Parse the JSON, scoped to `src/`

The JSON stream mixes dependency-crate warnings in with the workspace's own. Filter to
`clippy::redundant_clone` with a primary span under `src/`, then group:

```python
import json, collections
by_file = collections.Counter()
with open("tmp/clippy-clone-sweep.json") as f:
    for line in f:
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            continue
        if obj.get("reason") != "compiler-message":
            continue
        msg = obj.get("message", {})
        code = msg.get("code") or {}
        if code.get("code") != "clippy::redundant_clone":
            continue
        spans = [s for s in msg.get("spans", []) if s.get("is_primary")]
        if spans and spans[0]["file_name"].startswith("src/"):
            by_file[spans[0]["file_name"]] += 1

for fn, c in by_file.most_common():
    print(c, fn)
```

`msg["rendered"]` on the same object gives clippy's full diagnostic (the exact line, the
`.clone()` span, and the "this value is dropped without further use" note) — read a sample of
these before trusting the count; see the false-positive check below.

## 3. Verify a sample before trusting the count — this lint has a low but nonzero false-positive rate

`redundant_clone` reasons about liveness, not about types with custom `Drop`/aliasing semantics
via unsafe code, and it can occasionally be wrong across a macro boundary or when the "unused"
value is actually observed through something clippy's borrow analysis doesn't model (e.g. a
raw pointer taken earlier, or a value moved into a closure captured by reference). Read
2-3 `rendered` diagnostics per file before batching a fix, confirm by eye that the cloned value
really is never read again in that scope, and build (`cargo build`) after applying a batch rather
than trusting the diagnostic blindly.

The 2026-09-20 baseline sample (`src/value/error_typed.rs`, 64 hits — the single hottest file)
checked out as a real, repeated pattern: a `String` built with `format!()`, cloned once into an
error-attributes map, and never used again — `Value::str(msg.clone())` should be
`Value::str(msg)`. That is the shape most hits in this codebase have: build a message once, use
it in two places, but the second use only needed a move.

## 4. Decide: fix directly, or file issues

- **A handful of hits in one or two files** (roughly under ~15): just fix them in place, run
  `make test` (the affected area at minimum), and land it as one small PR — no issue needed.
- **A large sweep across many files** (the common case — the 2026-09-20 baseline was 1,088 hits
  across 226 files): file `todo:ticket` issues, one per module-sized cluster, rather than trying
  to fix everything in one PR or filing 226 issues. Balance cluster size against
  "small, self-contained, finishable in one session" (roughly 40-200 hits per cluster worked
  well): group by top-level `src/` subdirectory, and split the largest ones (`src/runtime` was
  498 hits alone) into sub-clusters by filename prefix (`methods_*`, `native*`, `registration*`/
  `resolution*`/`dispatch*`, `regex*`, `types*`/`accessors*`, everything else) so each ticket
  stays in that range. Follow the filing mechanics (labels, title-as-a-sentence, claim protocol)
  in `docs/issue-workflow.md`; use the `ticket.md` template's shape but adapt the "Repro" section
  — there is no Raku-level repro for a Rust-internal clone, so replace it with the exact clippy
  invocation plus the affected file list (paths only, not line numbers — they drift; whoever
  works the ticket re-runs the command to get current locations).
- Leave the `tier:*` label off when filing (per `docs/issue-workflow.md`, that is assigned by a
  later triage pass, not by the filer).
- **The 2026-09-20 sweep's own issues** are linked from `news/2026-09/` — check there (or search
  open `todo:ticket` issues whose title mentions "redundant clone" / "wasted `.clone()`") before
  filing a fresh batch, so a repeat sweep does not duplicate an already-filed cluster that just
  hasn't been picked up yet.

## 5. Fixing one of these tickets

Each hit is the same mechanical shape: replace `x.clone()` with a move of `x` (or, if the
variable is used only for that one clone and nothing else, drop the intermediate `let` and
move the original expression directly). After a batch in one file:

- `cargo build` to confirm it still compiles (a dropped `.clone()` can occasionally surface a
  real borrow-checker conflict if two "unused" values were actually aliased — that is real
  signal, not a false positive in the lint, and means that specific hit should be left alone).
- Re-run the clippy command scoped to that file to confirm the warnings are gone:
  `cargo clippy --all-targets -- -W clippy::redundant_clone 2>&1 | grep -A2 '<path>'`.
- `cargo fmt` and `make lint` before committing, per CLAUDE.md's Conventions section — a
  mechanical clone removal can still touch formatting/import warnings across the four lint
  configurations `make lint` gates on.
- This is Rust-internal code quality work, not a Raku-compat change, so it does not need a new
  `t/` test; the existing suite (`make test`) covers behavior, and `make lint` covers the removed
  clones' own concern (whether the resulting code is warning-free).
