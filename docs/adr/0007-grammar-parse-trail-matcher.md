# ADR-0007: Grammar/regex matcher — cursor + undo-log (trail) to kill capture-threading churn

- **Status**: Accepted (2026-07-16) — implemented in #4591
- **Context**: PLAN §5 grammar-parse performance. Follows the incremental Arc-shared
  sub-captures slice (#4586) and the exponential/ceremony fixes (#4556, #4559).

## Implementation outcome (2026-07-16, #4591)

The four phases landed as one PR: a key simplification made the split unnecessary.
Every candidate producer only ever *appended* to the cloned accumulated caps, so
converting them to delta output was mechanically `current_caps.clone()` →
`RegexCaptures::default()` (~30 sites) with the per-branch merge-field subsets preserved
exactly; the engine walk became a recursive DFS (`walk_tokens`) over a `CapStore`
(`regex_trail.rs`) with mark/apply-delta/rewind, and the all-ends producers stayed
materialized (their elements are now cheap deltas, so laziness — P2's concern — stopped
being load-bearing). `quant_expand_greedy` became the trail-based `quant_alt_dfs`.

Measured (local release A/B): deep bench ~×1.25, shallow ~×1.2; memmove 15.4% → 7.5% of
samples, `RegexCaptures::clone` 2.1% → 1.0%. The per-step O(accumulated) clone is
structurally gone — but the headline "~60–70% allocator churn" turned out to be only
partly *accumulated-state* churn. The remainder (~36% of samples post-trail) is
**per-subrule ceremony**, present on main too: candidate `Vec`s, captured-text `String`s,
`Arc<RegexCaptures>` subcap allocations, HashMap+SipHash traffic on the caps maps,
`RegexCaptures::default` zeroing (~2%), one `snapshot()` per complete inner end, and a
still-unexplained **runtime regex re-parse** path (`parse_regex_uncached` + LTM expansion
~4% in-profile). The ≥5× deep-bench target therefore needs the follow-up slices (find &
memoize the residual re-parse; FxHash or small-map for caps maps; box cold
`RegexCaptures` fields; intern trail-record keys) — tracked in PLAN §5.

## Problem

mutsu's grammar/regex matcher is **~30× slower than Rakudo on non-trivial grammar
parses**, and the gap is entirely a **constant-factor allocation-churn** problem, not
an algorithmic one.

### Measured characterization (release build, 2026-07-16)

Grammar: the JSON-like grammar from `benchmarks/bench-grammar-parse.raku`. Document:
an object of `P` pairs, each value a nested array of 8 `[i,i]` sub-arrays.

| P | chars | mutsu (single parse) | raku |
|---|------:|---------------------:|-----:|
| 2 | 111 | 2.04 s | 0.26 s |
| 4 | 221 | 4.17 s | — |
| 6 | 331 | 6.09 s | — |
| 8 | 441 | 8.11 s | 0.26 s |

The mutsu curve is **linear** (intercept ≈ 0, slope ≈ **18 ms per matched character**);
raku is flat here (startup/compile-bound at these sizes, steady-state per-char cost
negligible). So mutsu's asymptotics are fine — the *per-character work* is ~50–100×
too expensive. (An earlier note claimed O(n²)/"140×"; that was an artefact of a
3-iteration benchmark loop — corrected here and in PLAN/news.)

### Root cause — profile of a ~6 s parse

`perf` (release): **memmove ~19%, malloc ~17% + _int_malloc ~11%, free ~20%,
finish_grow (Vec realloc) ~12%**, `RegexCaptures::clone`+drop ~4% (down from ~10%
before #4586), `RegexCaptures::default` ~1.7%. So **~60–70% of the time is allocator
+ struct-copy churn**.

The cause is architectural: `RegexCaptures` (mod.rs ~L848, `#[derive(Clone, Default)]`,
~20 fields incl. 6 `HashMap`s / 7 `Vec`s, several hundred bytes) is **threaded by
value through the entire backtracking search and cloned at nearly every step**:

- Per **quantifier iteration**: `current_caps = new_caps.clone()` at
  `regex_match_core.rs:1096/1338/1419` — a full clone of the *accumulated* caps on each
  `*`/`+`/`**` iteration (locally O(iterations²)).
- Per **candidate end**: ~14 `current_caps.clone()` sites in `regex_match_capture.rs`
  and ~5 in `regex_match_atom.rs` (Alternation/SeqAlt/Group/CaptureGroup/Conjunction/
  GoalMatch/Named), one clone per alternative × per inner end.
- Per **separated-quantifier chain node**: `atom_caps.clone()/sep_caps.clone()` at
  `regex_match_core.rs:468` (clones whole per-iteration cap vectors).
- **Pure waste**: zero-width / leaf atoms clone the entire struct just to thread it
  unchanged (`regex_match_capture.rs:163,188,466,688`).

#4586 (Arc-shared nested sub-captures) removed the *deep-copy of sub-match subtrees*
(clone+drop 10%→4%), but the **top-level struct clone still deep-copies every `Vec`/
`HashMap` (and every `String` key)** at each step. That is the remaining churn.

## Decision

**Replace by-value capture threading with a single mutable capture store + an
undo-log ("trail"): mutate forward as the cursor advances, and rewind the recorded
deltas on backtrack.** This is how NQP/MoarVM (and classical backtracking engines)
work, and it makes per-step capture cost O(delta) instead of O(accumulated state).

Concretely:

1. **One mutable `RegexCaptures` per top-level match**, owned by the engine, not cloned
   per step.
2. **A trail** — a `Vec` of undo records. Each mutation pushes a record describing how
   to reverse it; `mark()` returns the current trail length; `rewind(mark)` pops and
   applies undo records back to `mark`. Because the caps mutations are overwhelmingly
   **append-only** (see the inventory below), most undo records are just "truncate Vec
   *k* to length *n*" or "truncate the per-key Vec / remove the key". Only the
   `$N=`/alias mid-vector overwrites (`regex_match_core.rs:626-686`) need value-level
   undo records.
3. **Convert the all-ends producers to depth-first-with-rewind generators.** This is the
   load-bearing structural change (see "Structural blocker").

### Why not the cheaper alternatives (rejected / insufficient)

- **Leave it (ship only #4586).** Rejected: still ~30× off raku; the churn is the
  headline grammar-parse cost and the batteries goal (§1) leans on real grammar parsing
  (JSON::Tiny-style, zef metadata). Worth the rewrite.
- **Box the cold `RegexCaptures` fields** (`Option<Box<ColdCaps>>`) to shrink the struct
  and cut memmove. A contained win (~another single-digit %), but it does **not** remove
  the per-step *malloc/free* of the hot `named`/`positional` collections — it only
  shrinks the header memmove. Keep as a possible *complementary* micro-step, not the fix.
- **Persistent/immutable caps (`im`/HAMT) with structural sharing.** Would make clones
  cheap without a trail, but adds a dependency, changes every access site, and its
  per-op constant factor is worse than plain `Vec`/`HashMap` for the common small maps.
  The trail keeps the hot data structures as-is and pays only for real mutations.
- **Full CPS→bytecode regex VM (NQP-style compiled regex).** The eventual ceiling, but
  far larger than this ADR; the trail rewrite is the prerequisite step that de-risks it.

## Current architecture (what the rewrite touches)

Two backtracking mechanisms coexist (full map in the investigation notes):

1. **Explicit LIFO stack** of `(idx, pos, RegexCaptures)` inside
   `regex_match_ends_from_caps_in_pkg_impl` (`regex_match_core.rs:787`). A single linear
   walk over `pattern.tokens`; each token pushes its continuations. **This is where the
   cursor + trail lives** — it is already a stack machine; it just carries caps by value.
2. **All-ends enumeration**: functions returning `Vec<(usize, RegexCaptures)>` — the
   atom matcher (`regex_match_atom_all_with_capture_in_pkg`, atom.rs:45), the engine's
   own `regex_match_ends_from_caps_in_pkg`, the separated-quantifier chain builders,
   `quant_expand_greedy`, `build_named_candidates_from_inner`. Each element is a
   *different capture state at the same call site*, all materialized at once.

Single-match functions (`regex_match_end_from_caps_in_pkg`,
`regex_match_atom_with_capture_in_pkg`) are already "first end only" and map cleanly onto
"advance cursor, keep first success" — the easy cases.

The **ratchet fast paths** (`regex_match_core.rs:934-1028`, `:1125-1252`) already thread
only `pos` (or a single moved caps) with no cloning. **They are the model the trail
generalizes** — preserve/subsume them, don't rewrite them.

### Structural blocker (the crux)

The trail assumes **one path explored at a time**. But the all-ends producers hand back
N divergent capture states simultaneously (pushed onto the LIFO stack together at
`regex_match_core.rs:814,848,893,917,1111,1117,1354,1360,1440,1446`). A single mutable
store cannot hold N states at once. Each fan-out point must become: `let m = mark();`
apply candidate *i* → descend to the next token → on backtrack `rewind(m)` → apply
candidate *i+1*.

The producers that genuinely require *all* ends (must become lazy generators over
`(end, trail-delta)` rather than materialized owned-caps vectors):

- **LTM longest-match `|`** (atom.rs:58) — evaluate every alt, pick longest, keep all for
  outer backtracking.
- **`||` sequential alternation** (atom.rs:92) — expose all lengths for recursive rules
  (`r = <?> || x <r>` needs every r-length for an outer `$`).
- **Outer-anchor backtracking into groups/subrules** (atom.rs:175,360) — a following
  `$`/goalpost may only be satisfiable at one specific inner length.
- **Frugal atoms / separators expanding for a following anchor** (core.rs:362 separated
  chains, `quant_expand_greedy` core.rs:55) — shortest match must be able to grow.
- **Conjunction `&`/`&&`** (atom.rs:141) — needs all first-branch ends to find one where
  every other branch also ends.
- **LTM protoregex / left-recursion seed growth** (atom.rs:424, `LR_MEMO`/`LR_ACTIVE`).

### Hazards the trail must respect

- **Thread-local side channels are NOT caps fields and must NOT be rewound**:
  `LR_MEMO`/`LR_ACTIVE` (atom.rs:15), `EAGER_CODE_BLOCKS` (capture.rs:253),
  `PENDING_REGEX_ERROR`/`PENDING_REGEX_GOAL_FAILURE` (errors/goal-failures deliberately
  persist across backtracks). `REGEX_PRECEDING_CHAR` is already RAII-guarded. The trail
  must explicitly exclude these.
- **`apply_named_capture` mid-vector truncation/overwrite** (`$N=`/alias,
  core.rs:626-686) is the one place a length-only truncate cannot rewind — needs
  value-level undo records.
- **`Arc::make_mut` on a stored subcap** (atom.rs:689, sets `action_name`) mutates
  through the Arc — the trail record must restore the prior `action_name`, or (simpler)
  this write moves before the Arc is shared.
- **Sub-interpreter spawning** on Named/Code/VarDecl paths clones `self.env`+registry —
  orthogonal to caps churn but part of the per-subrule cost; out of scope here, noted for
  a later slice.

### Trail-record shape (from the mutation inventory)

Field writes are grouped in the investigation notes. The undo vocabulary is small:

- `positional*` family, `code_blocks` — **append-only Vec growth** → undo = truncate to
  saved length. (Plus the `$N=`/alias truncate-overwrite exceptions above.)
- `named` / `named_subcaps` / `hash_captures` — **per-key Vec append** → undo = truncate
  that key's Vec to saved length, or remove the key if newly created.
- `named_quantified` (HashSet) — undo = remove-if-newly-inserted.
- `sym`, `action_name`, `capture_start`/`capture_end`, `from`/`to` — scalar → undo =
  restore prior value.
- `matched`/`match_from` — set once at init / on subcap finalize, not mid-search.

## Consequences

- **Gain (per CLAUDE.md gain/risk):** removes the dominant band-aid (by-value CPS
  threading) and makes the matcher *sound and fast* — the churn cannot come back because
  there is no per-step clone. Target: bring grammar-parse per-char cost from ~18 ms/char
  toward raku's steady state (aim ≥5× on the deep bench; the shallow bench should also
  improve markedly). Directly serves the batteries goal (real grammar parsing).
- **Risk:** it is a large, high-blast-radius matcher change. The semantics surface is
  wide (capture markers `<( )>`, silent-action channel, aliases, `@<>`/`%<>` sigil
  captures, `$N=`, LTM/`||`/`&`, ratchet, frugal, separated `%`/`%%`, code blocks,
  backrefs, left recursion). Mitigation: CI's full roast S05 suite (67 whitelisted files,
  2610 subtests) + the `t/` grammar/match suite are the safety net; the change lands on a
  branch and fixes forward. This is exactly the "refactor boldly — CI + roast are the
  safety net" case.

## Phasing (proposed, to be confirmed before implementation)

1. **P0 — scaffolding, behavior-preserving.** Introduce `struct CaptureTrail { marks }`
   and `mark()/rewind()` next to a single owned `RegexCaptures`, but keep the existing
   all-ends producers; use the trail only in the linear LIFO token walk where a single
   path is already followed (ratchet-dominated grammars). Prove no regression + measure.
2. **P1 — depth-first single-match core.** Convert `regex_match_ends_from_caps_in_pkg_impl`
   to advance a cursor with `mark/rewind` for the common ratchet / single-candidate case;
   fall back to the old all-ends path for the genuine multi-candidate producers.
3. **P2 — lazy generators for the all-ends producers** (LTM `|`, `||`, groups, separated
   chains, conjunction, LR): replace `Vec<(end, caps)>` with an iterator yielding
   `(end, trail-mark)` so backtracking rewinds instead of discarding cloned states.
4. **P3 — remove the by-value `RegexCaptures` threading** and the now-dead clone sites;
   optionally box remaining cold fields.

Each phase is independently shippable and roast-gated. Re-baseline against
`benchmarks/bench-grammar-parse.raku` (shallow) and `bench-grammar-parse-deep.raku`
(deep) at every phase; record numbers from bench CI, not local runs.

## References

- #4586 (Arc-shared sub-captures) — news/2026-07.md, PLAN §5.
- Investigation map: the call graph, full clone inventory, and per-field mutation
  inventory are summarized in this ADR; the exhaustive file:line list lives in the
  2026-07-16 session notes and can be regenerated by reading `src/runtime/regex/`.
- Benches: `benchmarks/bench-grammar-parse.raku` (shallow, 231 chars),
  `benchmarks/bench-grammar-parse-deep.raku` (deep, object-of-nested-arrays).
