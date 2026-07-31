# ADR-0016: Span-based regex captures and lazily materialized `Match` objects

- **Status**: Proposed (2026-07-28)
- **Context**: ADR-0001 Phase A (single-thread speed catch-up, before GC/JIT).
  Direct follow-on to ADR-0007, which removed the *accumulated-state* clone from the
  matcher and explicitly deferred the remaining "**per-subrule ceremony**" — captured-text
  `String`s, `Arc<RegexCaptures>` subcap allocations, `RegexCaptures::default` zeroing,
  one `snapshot()` per complete inner end. This ADR is the decision about how that
  ceremony is removed.
- **Ticket**: `todo/tickets/yaml-parse-throughput.md`. Benchmarks:
  `benchmarks/bench-yaml-parse.raku`, `benchmarks/bench-grammar-parse{,-deep}.raku`.

## Problem

After three targeted fixes (regex code-block writeback by identity, grammar-action
dispatch pre-check, `.orig` `Arc` sharing) a clean profile of a grammar parse has **no
single dominant function left**. It is dominated by libc:

| symbol | share |
|---|---:|
| `__memcmp_avx2_movbe` | 11.0% |
| `_int_free` | 7.5% |
| `malloc` | 5.4% |
| `_int_malloc` | 5.3% |
| `__memmove_avx_unaligned_erms` | 5.2% |
| `cfree` | 2.9% |
| `realloc` | 2.7% |
| `malloc_consolidate` | 1.7% |
| `unlink_chunk` / `__rdl_alloc` | 2.2% |

(`benchmarks/bench-yaml-parse.raku`, `--profile profiling`, `perf -F 4000`, 20 184
samples, on an idle box — "idle" is load-bearing here; see the measurement caveat in
`todo/tickets/yaml-parse-throughput.md` for the session whose local A/B numbers a
3-day-old CPU spinner invalidated. Shares are *shape* evidence, not an A/B claim;
magnitudes for any fix must come from `bench-history.tsv` on `bench-data`.)

**≈28% allocator + ≈5% bulk memmove + ≈11% memcmp.** That is not a hot function to fix;
it is a data model that allocates and copies per match step. This ADR names the five
structural causes and commits to the target representation.

### Cause 1 — the subject string is re-sliced per subrule, so every capture subtree is deep-copied to rebase it

A subrule call matches its body against `&chars[pos..]` with `start = 0`
(`regex_match_atom.rs:377`, `regex_match_capture.rs:548`). Every offset the body
produces is therefore **slice-relative**, and the caller rebases the whole result:

```rust
Self::shift_capture_descendants(&mut subcap, pos);   // regex_match_atom.rs:764
```

which recurses with `Arc::make_mut(sub)` over every nested named/positional subcapture.
Those `Arc`s are **already shared** — `record_reduced_subrule` clones each one into the
`REDUCED_SUBRULES` thread-local at the moment it is built — so `make_mut` is not the
cheap unshared path: it **deep-clones the entire descendant subtree, at every level of
nesting, on every candidate**, including candidates that go on to lose. A depth-`d`
parse copies its capture tree `d` times.

Two more workarounds exist only because of the slicing: `REGEX_PRECEDING_CHAR` (a
thread-local + RAII guard whose entire job is to tell a slice what character preceded
it, so `^^` keeps working) and the slice-relative `capture_start`/`capture_end` rebasing.
Everything the workaround does *not* cover is simply **wrong** under it: `<<`/`>>`/`<?wb>`
read `chars[pos - 1]`, which is "nothing" at slice position 0, so a word boundary fires
mid-word; a look-behind inside a subrule cannot see text before the subrule's start; and
`<at(N)>` means "position N in the current slice" rather than in the subject. All four
were verified against `raku` and fixed by P1.

### Cause 2 — captures store text, not spans, so positions are recovered by searching

`RegexCaptures::named` is `HashMap<String, Vec<String>>` — a named capture keeps its
**matched text** and no offsets. So the Match builder searches for the text in the
subject to recover them (`value_methods_c.rs`, `make_capture_match`):

```rust
let needle: Vec<char> = s.chars().collect();
for start in search_from..=haystack.len().saturating_sub(needle.len()) {
    if haystack[start..start + needle.len()] == needle[..] { ... }
}
```

A fresh `Vec<char>` per leaf, and a linear scan that is O(document) when the text does
not occur near `search_from` — over a leaf-heavy tree (YAMLish produces one leaf per
matched character in a run of spaces) that is O(captures × document) `memcmp`. It is
also **semantically wrong**: it finds the first occurrence at or after a heuristic start,
not the span that actually matched, so repeated text yields wrong `.from`/`.to`.

The positional axis already records exact spans in `positional_offsets` — **and the Match
builder ignores them and searches anyway.**

A gated experiment (skip the search, keep everything else) moves `__memcmp_avx2_movbe`
from 11.0% to 6.9% of samples at equal wall clock, so the search is worth ≈4 points on
its own. The remaining ≈7% is the *other* string comparison in this model: owned `String`
capture-name keys compared on every `HashMap` probe and trail record (Cause 4). Neither is
a single fix — which is the point.

The band-aid this grew: `store_apply_named_capture` synthesizes a whole
`Arc<RegexCaptures>` per named capture *purely as an offset carrier* ("Record a minimal
sub-capture carrying the exact (from, to) span"), because `named` has nowhere to put two
integers.

### Cause 3 — one struct plays two roles, so a stored leaf node costs ~600 bytes

`RegexCaptures` is simultaneously

- the engine's **mutable accumulator** for the current pattern run — `code_blocks`,
  `regex_vars`, `match_from`, `capture_start`/`capture_end`, and the trail's target; and
- an immutable **stored capture node** — what an `Arc<RegexCaptures>` subcap is.

A stored node needs `from`, `to`, its children, `ast`, `sym`, `action_name`. It gets all
14 fields: 5 `HashMap`s + 1 `HashSet` + 7 `Vec`s + a `String` ≈ 600 bytes, zeroed by
`RegexCaptures::default()`, moved by value inside `Vec<(usize, RegexCaptures)>` candidate
lists, heap-allocated per subcap, and `clone()`d once per complete match (`snapshot()`).
That is the `memmove` and a large part of the `malloc`/`free`.

### Cause 4 — six parallel vectors per axis

The positional axis is `positional` ‖ `positional_subcaps` ‖ `positional_quantified` ‖
`positional_offsets` ‖ `positional_nil` ‖ `positional_slots`; the named axis is `named` ‖
`named_subcaps` ‖ `named_quantified`. Six allocations, six trail-record families, six
truncate/extend paths per rewind — and manual alignment invariants of the form "pad with
`false` up to the current `positional` length before pushing `true`". Every capture name
is stored as an owned `String`, re-allocated per `entry(key.to_string())` and per trail
record.

### Cause 5 — the `Match` tree is materialized eagerly, in the heaviest possible shape

Every capture — including a leaf that matched one space — becomes a full
`Value::Instance("Match")`: a `HashMap<String, Value>` of six entries (`str`, `from`,
`to`, `list`, `named`, `orig`) built with six `String` key allocations, converted to a
`Symbol`-keyed `AttrMap`, wrapped in a GC-managed `InstanceAttrs`, plus an empty `list`
array and an empty `named` hash **per leaf**. Nothing is lazy: the entire tree is built
whether or not user code ever looks at it.

The consumer side then pays for that shape again. `invoke_grammar_actions` walks the
finished tree and, per node, does `attributes.as_ref().clone()` (full `AttrMap` copy) and
`named_hash.as_ref().clone()` (full `HashData` copy) to rebuild the node immutably — an
O(node) copy per node, i.e. O(n²) over the tree, for a walk that in the common case
changes nothing (ADR round 2 established that most nodes have no action method at all).

## Decision

Adopt the representation NQP/MoarVM uses and that Raku's own semantics assume: **the
matcher carries spans into one shared, immutable subject; the Raku-level `Match` object is
materialized lazily and only where it is observed.**

Concretely, the target state is:

1. **Absolute positions everywhere.** The subject is never re-sliced. A subrule is matched
   against the full `chars` with an explicit start position. Offsets are absolute from
   birth; nothing is ever rebased.
2. **One shared subject.** `MatchTarget { text: Arc<String>, chars: Arc<[char]> }`, created
   once per top-level match/parse and referenced by every node. `.orig` is a refcount bump;
   any capture's text is `&target.chars[from..to]`, materialized on demand.
3. **A capture node distinct from the engine's accumulator.** `CapNode` — the immutable
   stored node: `{ from: u32, to: u32, children: Option<Box<CapChildren>>, ast, sym,
   action_name }` — split out of `RegexCaptures`, which keeps only the per-run mutable
   state (`code_blocks`, `regex_vars`, `capture_start`/`capture_end`, `match_from`) and the
   in-progress children. A leaf `CapNode` is a handful of words, not ~600 bytes.
4. **One list per axis, spans not text.** `positional: Vec<PosSlot>` and
   `named: HashMap<Symbol, Vec<Arc<CapNode>>>` replace the six/three parallel collections.
   The text axis disappears (it is derivable), which is what makes the collapse possible.
   Capture names are interned `Symbol`s, not re-allocated `String`s.
5. **Lazy `Match`.** A dedicated `ValueRepr::Match(Gc<MatchNode>)` holding the target, the
   span, and the `Arc<CapNode>`; `.list`/`.hash`/`.Str` are derived on first access and
   memoized. A capture nobody inspects costs nothing. The grammar-action walk runs over
   `CapNode` and materializes a `Match` only for nodes that actually have an action method.

### Non-goals

- No change to the backtracking algorithm. ADR-0007's trail/`CapStore` stays; this ADR
  changes *what* the trail records, not *how* backtracking works.
- No change to observable Raku semantics, except where the current model is provably
  wrong — search-recovered offsets for repeated text, and the four subrule-boundary
  constructs above — which become correct.
- Not a compiled-regex VM. That remains the eventual ceiling (ADR-0007's own framing) and
  is easier to reach on top of this representation, not instead of it.

## Phasing

Each phase is independently shippable, roast-gated, and measured from `bench-history.tsv`
on the `bench-data` branch — never from a local A/B.

**P1 — Absolute positions.** Pass `(chars, pos)` instead of `(&chars[pos..], 0)` at the two
subrule call sites; delete `shift_capture_descendants` / `shift_capture_tree` and the
`Arc::make_mut` subtree deep-copy they exist to perform, and `REGEX_PRECEDING_CHAR` with
its guard.
*Why first:* it is the largest single copy source, it is localized (two call sites plus the
shared `build_named_candidates_from_inner` wrapper), and **spans are meaningless until
positions are absolute**, so P3 depends on it.
**Landed 2026-07-28** (`news/2026-07/regex-subrule-absolute-positions.md`): net −104
lines, and the four subrule-boundary constructs above now agree with `raku`, pinned by
`t/regex-subrule-absolute-position.t`.

*Measured (bench CI, `bench-history.tsv` on `bench-data`, `afc3475e2` vs the two
preceding main commits).* Raw medians are not comparable — runner speed swung ~18%
between the two pre-P1 runs, and P1's runner was the slow one (`int-arith` +38%,
`mandelbrot` +41%, neither of which this change can touch). Normalizing each benchmark
by `int-arith` on its own runner makes the two pre-P1 points agree to within 1–3%, so
the ratio is usable:

| benchmark (÷ `int-arith`) | `0eb479d9` | `17af2292` | `afc3475e` (P1) |
|---|---:|---:|---:|
| `bench-grammar-parse` | 0.370 | 0.369 | **0.301** |
| `bench-grammar-parse-deep` | 0.311 | 0.320 | **0.250** |
| `bench-yaml-parse` | 117.4 | 116.9 | **121.5** |

So ≈−20% on both grammar benchmarks and ≈+4% on the YAML one. The −20% is well
supported (two independent pre-change points agree to 1%, and both grammar benchmarks
move the same way); the +4% is a single point and within the noise this benchmark shows
locally, so **treat it as "no measured change", not as a regression** until more
`bench-data` rows accumulate.

*A first hypothesis for the split was tested and rejected.* `REDUCED_SUBRULES` is armed
only for a `.parse(:actions(...))`, which YAMLish uses and the grammar benchmarks do not
— so it was plausible that P1's deep copy had merely **moved** to
`reduce_regex_captures_made_for_rule` (which does `Arc::make_mut(sc)` on every child)
because the log keeps every node shared. Measured with an env gate that makes
`record_reduced_subrule` a no-op, interleaved and pinned to one core, 5 pairs:
median **4.11 s with the log vs 4.15 s without** — no difference. The control
(`bench-grammar-parse`, no actions, where the gate must be inert) behaved as expected.
So the log is not a measurable cost here, and that mechanism does not explain the split.
Note the experiment does not disprove reduce-walk copying in general: `snapshot()` also
clones the `Vec<Arc<..>>` per complete match, so nodes are shared with or without the
log. **P2's first task is therefore to count, not guess** — instrument the reduce walk
with an `Arc::strong_count(sc) > 1` counter before each `make_mut` and report it under
`MUTSU_VM_STATS`.

(`bench-yaml-parse` was checked to be a valid instrument for match time: `use YAMLish`
alone is 0.04 s of its ~4.07 s, so it is not module-load dominated.)

**P2 — `CapNode` / `RegexCaptures` split.** Extract the immutable stored-node fields into
`CapNode`; `Arc<CapNode>` replaces `Arc<RegexCaptures>` in both subcap axes. A *leaf* node
collapses its child collections to a single `None` (`children: Option<Box<CapChildren>>`),
which is where the order-of-magnitude shrink comes from; it also takes the
`Vec<(usize, RegexCaptures)>` candidate-list memmove with it.
**Landed 2026-07-31** (`news/2026-07/regex-capnode-split.md`): `CapNode` =
`matched`/`from`/`to`/`sym`/`action_name`/`ast` + `children: Option<Box<CapChildren>>`,
≤112 bytes for a leaf (pinned by `cap_node_size_guard`); conversion via
`RegexCaptures::into_cap_node()` at the ~10 storage sites; the reduce walk and the
failed-parse replay recurse over `CapNode`; the accumulator-only fields
(`hash_captures`, `positional_slots`, `positional_offsets`, capture markers,
`match_from`) are dropped at conversion — nothing ever read them through a stored node.
Note: the candidate lists themselves still move `RegexCaptures` deltas (ADR-0007's
delta protocol); their per-delta cost now shrinks with P4's axis collapse rather than
with this phase.
**Start by counting the reduce-walk copies** (see P1's measurement note — guessing at
this already cost one rejected hypothesis): every `Arc::make_mut` on a *stored* node is a
deep copy whenever anything else holds a handle, and `snapshot()` alone is enough to make
that so. Two such sites exist today — `reduce_regex_captures_made_for_rule`'s per-child
`make_mut`, and the `action_name` write in `build_named_candidates_from_inner`, which
happens *after* `Arc::new` and after `record_reduced_subrule` has cloned the handle, so
for every aliased subrule capture (`<str=single-bare>`, which YAMLish uses heavily) it is
a guaranteed subtree copy. That one is avoidable outright by setting `action_name` before
`Arc::new`: the value is `spec.lookup_name`, the same name the log records, so the
failure-path replay (`get_action_name()` falling back to the logged rule name) is
unaffected. Fold it into P2 rather than shipping it as a perf claim — the log experiment
suggests this class may sit below measurement noise, and the `CapNode` design removes it
structurally anyway: a genuinely immutable node with `ast`/`made` and `action_name`
attached out-of-band (a side table keyed by node identity, or `OnceCell`) rather than
written through the `Arc`.

**P3 — Spans, not text.** Introduce `MatchTarget`; `CapNode` carries `(from, to)` only.
Delete `chars[a..b].iter().collect()` at capture sites, the `captured.clone()` duplicates,
and `make_capture_match`'s search entirely — the Match builder reads the recorded span.
Fixes the repeated-text offset bug.
*Measured 2026-07-31 (counted, not guessed — post-P2 `leaf_searches`/`leaf_spans`
counters under `MUTSU_VM_STATS`):* the position search is **already dead on every
workload tested** — `bench-yaml-parse` (0 searches / 38,431 span reads), the YAMLish
battery test, roast S05 capture/global files, and ad-hoc `m:g`/`split`/`subst` shapes
all report `leaf_searches=0`, because every leaf reaching the builder now carries a
span-bearing `CapNode` (P2 made the `store_apply_named_capture` carrier the natural
representation). The pre-P1 "≈4 points of memcmp" attributed to the search no longer
applies. **P3's value is therefore the text axis itself**, not the search: per stored
leaf the text is materialized and copied up to three times (`chars[a..b].collect()`
into `CapNode.matched`, again into the accumulator's `named`/`positional` text vecs,
again into the Match `str` attribute) — 38k `String` allocations per bench parse.
The search fallback is retired when the text axis goes.

**P3a landed 2026-07-31** (`news/2026-07/regex-match-target-span-reads.md`): the
subject became `MatchTarget { text: Arc<String>, chars: Arc<[char]> }`, built once
per engine entry, published on the returned accumulator
(`RegexCaptures::target`) and carried by every lazy `MatchNode`; a thread-local
engine scope covers mid-match synthesis (`<?{ }>` `.made` dispatch, reduce-time
`$*` actions). On top of it the **stored text axis is gone where a span already
lived**: `CapNode.matched` and `RegexCaptures.matched` are deleted (readers
derive text via `span_str`, with an ASCII byte-slice fast path),
`QuantifiedCaptureEntry` is `(from, to, subcap)`, `positional_slots` is spans
only, and the P5 leaf position search is retired outright. Landing this
surfaced that `positional_offsets` was NOT maintained by every positional
producer (the quantified-fold fallback fabricated `0..len` — exactly the
Cause-2 class of bug); every push/merge site now keeps the axis aligned.
Compatibility fixes on the way: `:m`/`:i`-fold captures are remapped to
original-subject space recursively (sub-captures previously reported derived-
space offsets and derived-space text — `m:m/ caf (e) s /` on "cafés" now
captures "é" like raku), and the pcre2 `:P5` path reported byte offsets where
char offsets were expected. What P3 still owes: the accumulator's
`named`/`positional` text vectors (and `CodeBlockContext`'s snapshot copies),
which are structurally the P4 axis collapse — local interleaved A/B shows the
intermediate state ≈ +2–3 % on `bench-yaml-parse` (the capture-site text
`collect`s still run, plus span-derived reads), to be recovered when the text
vectors disappear with P4.

**P4 — One list per axis + interned names.** Collapse the parallel vectors/maps into
`Vec<PosSlot>` and `HashMap<Symbol, Vec<Arc<CapNode>>>`, and shrink the trail's undo
vocabulary accordingly (the alignment invariants become structural rather than asserted in
comments).

**P5 — Lazy `Match`.** First a pure refactor: funnel the `class_name == "Match"`
consumer sites through accessor helpers (`match_str` / `match_span` / `match_list` /
`match_named`) with no behavior change. Then swap the representation behind them to
`ValueRepr::Match(Gc<MatchNode>)` with `OnceCell`-memoized derived views, and rewrite
`invoke_grammar_actions` to walk `CapNode` and materialize only where `has_user_method`
already says an action exists — which removes the per-node `AttrMap`/`HashData` clone as
a side effect.
*Scoped 2026-07-31* (`todo/deep/adr0016-p5-match-consumer-inventory.md` — the full
sweep): the "~34 sites" estimate was low — **72 attribute-touching sites** (9 builders,
7 clone-insert-rebuild sites, 18 scalar readers, 37 `list`/`named` structure readers,
1 in-place mutator), plus two attributes this ADR had not listed (`actions`,
`__failed_match__`/`pos` on failed `.subparse` matches). The four proposed accessors
cover 55 of the 72; three more close the set (`match_ast`, `match_meta`,
`match_is_failed`). The builders already funnel through three constructors, so the
builder-side swap is cheap; the rebuilders need a `with_ast(...)`-style copy-on-write
helper first, and the one in-place mutator (`smart_match.rs`) must convert to the
rebuild pattern.
**Landed 2026-07-31** (`news/2026-07/regex-lazy-match-repr.md`): the repr is
`ValueRepr::Match(Gc<MatchNode>)` where `MatchNode = { orig: Option<Arc<String>>,
cap: Arc<CapNode>, id, attrs: OnceLock<Gc<InstanceAttrs>> }`. Rather than adding a
`ValueView::Match` arm (which would have forced every consumer to change), `view()`
materializes the memoized one-level attribute map and presents
`ValueView::Instance("Match")` — so consumers are unchanged and laziness is
preserved exactly where the seam accessors and tag probes keep `view()` from
running. Children materialize as lazy Matches themselves (one `Gc` alloc each).
The action walk skips actionless nodes via a capture-node peek and runs
leaf actions (`make ~$/` per char, the YAMLish shape) against the lazy `$/`,
applying `make` as a fresh lazy node. Two consequences worth recording:
(1) the "grammar-action walk materializes only where `has_user_method` says so"
plan met reality — YAMLish DOES define per-char leaf actions (`space`,
`single-bare`), so the win there came from the leaf-action fast path, not from
skipping; (2) keeping a lazy value alive across generic dispatch required
converting ~20 `matches!(v.view(), …)` variant probes into pure tag probes —
a `view()`-based "is it an X?" check is now an anti-pattern anywhere a lazy
Match can flow. Measured by instrumentation: leaf materializations on a YAMLish
parse fell 1807 → 15; local interleaved A/B (idle box): `bench-yaml-parse`
1.31 s → 0.87 s (≈ −34 %), bench CI to confirm.
The reduce walk (`reduce_cap_node_for_rule` code-block replay) and the failed
partial-parse replay still build eager Matches for `$/` inside in-regex code
blocks — small counts, unchanged semantics.
*Confirmed by bench CI (int-arith-normalized, `bench-history.tsv`):* the seven
main-branch points before the P5 merge sit at 38.2–41.8 (`bench-yaml-parse` ÷
`int-arith`); the P5 merge row (`fa2400a49`) is **25.65** — ≈ −35 %, matching
the local A/B.

## Consequences

- **Gain** (per CLAUDE.md's definition — moving toward the correct architecture): the
  per-match-step allocation and copying are removed *structurally*, not shaved. There is no
  per-capture text copy to regress, no subtree to rebase, no offset to re-derive by
  searching, and no `Match` object for a node nobody reads. Five real compatibility bugs
  are fixed on the way (the four subrule-boundary constructs, plus search-recovered
  offsets for repeated text). The representation is also the one a future compiled-regex VM and JIT
  want (spans + a shared subject + a lazy user-facing object).
- **Risk**: high blast radius across `src/runtime/regex/` (~10k lines) and, in P5, across
  the ~34 `Match`-consuming sites. The semantics surface is wide (capture markers
  `<( )>`, silent-action channel, aliases, `@<>`/`%<>` sigil captures, `$N=`, LTM / `||` /
  `&`, ratchet, frugal, separated `%`/`%%`, code blocks, backrefs, left recursion). The
  mitigation is the standing one: CI's roast S05/S12 suites plus the `t/` grammar/match
  suite, land per phase on a branch, fix forward. A temporary red CI on a feature branch is
  not a risk (CLAUDE.md, "Refactor boldly").
- **Rejected alternatives**:
  - *Keep the eager `Instance` tree, just build the `AttrMap` directly with pre-interned
    `Symbol`s.* A real but shallow win (six `String` allocations per leaf) that leaves every
    structural cause in place and entrenches the eager tree. Fold it into P5 instead of
    shipping it as the answer.
  - *Add a parallel `named_offsets` map so the builder can stop searching.* Fixes the
    symptom by adding a seventh parallel collection — the exact anti-pattern P4 exists to
    remove.
  - *Persistent/immutable capture structures (HAMT) instead of the trail.* Already rejected
    in ADR-0007 and unchanged here: worse constant factor for the small maps involved.
  - *Jump straight to a compiled-regex VM.* Larger than this ADR and strictly easier on top
    of it; the representation is the prerequisite, not the competitor.

## References

- ADR-0007 (trail matcher) — the immediate predecessor; its "residual per-subrule ceremony"
  paragraph is the problem this ADR solves.
- ADR-0001 — phase order: this is Phase A work, not blocked on GC or Track B.
- `todo/tickets/yaml-parse-throughput.md` — measurements and the three landed rounds.
- `news/2026-07/match-object-orig-arc-share.md`, `.../regex-code-block-writeback-by-identity.md`,
  `.../grammar-actions-skip-dispatch-for-missing-methods.md`.
