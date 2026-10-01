# ADR-10488: Capture levels are written in place, not assembled from per-call deltas

- **Status**: Accepted (2026-10-01). Implemented for the compiled engine (§5).
- **Issue**: [#10488](https://github.com/tokuhirom/mutsu/issues/10488) (`todo:perf`: a grammar parse
  makes ~186,000 allocator calls for a 10 KB document).
- **Supersedes in part**: [ADR-0016](0016-span-based-captures-and-lazy-match.md) Decision 4 (the
  named axis as a hash map of node vectors). ADR-0016's spans, shared subject, `CapNode` and lazy
  `Match` stand.
- **Relates to**: [ADR-0007](0007-grammar-parse-trail-matcher.md) (the capture store and its undo
  trail, kept), [ADR-0135](0135-regex-compiles-to-a-backtracking-program.md) (the compiled engine
  this changes; its D4 single-definition rule and D6 differential mode bound what changes now).

## 1. Context

ADR-0135 Slice D measured that a grammar parse spends about 21% of its instructions in `malloc`,
`free` and `memcpy`, and that the allocations are the match tree. Measured again on `main`
(0ece0536, release, `scripts/bench-det.sh benchmarks/bench-grammar-parse-big.raku`):

| | allocations |
|---|---:|
| the benchmark | 192,837 |
| the same file with the parse removed (start-up, building the document) | 28,222 |
| **the parse** | **~164,600** |

The parse builds about 8,300 subrule Match nodes, so it paid about **20 allocations per node**. By
call site (`cg-summary.py --allocs` over a callgrind run, one parse; the rows overlap):

| source | allocations |
|---|---:|
| `NamedSlot` hash maps: a table per level and per delta (25.3k), and their copies (19.2k) | ~44k |
| node vectors growing in `merge_delta`, `NamedSlot::merge`, `append_separated_captures` | ~18k |
| a separated quantifier's per-iteration levels: `collect`'s snapshot, `drain_collected`'s copy, the fold | ~15k |
| frames (`Rc<Frame>`), a proto call's ranking `Vec` | ~13k |
| a proto return's `:sym` (a `String` copy into a freshly allocated `RareCaps`) | ~8k |
| the nodes themselves (`Arc<CapNode>`, `Box<CapChildren>`) | ~14k |

None of these is the tree the parse returns; most are the *way it is put together*. ADR-0016 kept
ADR-0007's delta protocol as the interface between capture levels, and ADR-0135's compiled engine
inherited it: a subrule's return built a `RegexCaptures` delta with a map and a node vector of its
own, `merge_delta` copied the node out of it into the caller's map (allocating that map's table
and that name's vector on first use), and the delta was freed. A separated quantifier matched each
iteration in a capture level of its own, copied it twice and folded the copies side by side, even
when the only captures were names that need no folding; a `~` goal match did the same with its two
sides. And three ops recomputed, as interned strings, name sets that are a function of the
pattern.

## 2. Decision

**A capture level is written in place: what a subrule, an iteration or a quantifier contributes is
filed into the level that owns it, in filing order, with no intermediate delta; what can be known
from the pattern is computed when the pattern compiles.**

### D1. The named axis is a small map in filing order

`NamedCaptureMap` is a vector of `(name, slot)` pairs searched linearly, not a hash map, and a
slot's nodes (`CapNodes`) hold one node inline, spilling to a vector only for a name filed twice.
A level holds a handful of names, so a linear probe beats hashing, and an empty level allocates
nothing. Iteration order is filing order (match order), where it was a hash order; `.hash` builds a
hash of its own, so this is not observable there, and every comparison that depends on order
(ADR-0135 D6's `node_span`) already sorts.

### D2. A subrule's return files in place

Whether and where a subrule's match is filed (the capture name, the rule's own name for a
non-suppressing alias, the silent-action marker, or nothing but its `make` value) is decided once,
by `file_named_candidate`, and written through a `CapSink`. The walk's sink is a fresh delta, as
ADR-0007 requires of its candidate producers; the compiled engine's is the caller's `CapStore`,
whose writes are trailed. A callee frame's return therefore costs the node and nothing else.

### D3. Capture shape known from the pattern is compiled, and only where it is exact now

- The name sets `QuantNames`, `SepEmit` and `ZeroArm` mark are computed and interned when the
  program compiles (`RxProgram::name_sets`, `ZeroArmPlan`), sorted by name so the marking order
  does not depend on a hash seed.
- A **separated quantifier** gives each atom and separator a level of its own so that their
  captures fold side by side afterwards (`SepEmit`): positional slots per iteration into lists,
  every atom's names before every separator's. When neither side files a positional capture or a
  `<(`/`)>` marker at its own level (no capture group, numbered alias, lookaround, conjunction or
  goal match there) and the two sides can file **no name in common**, the fold's result is what
  filing in place gives, so the iterations file into the enclosing level and `SepNames` marks
  quantified the token's names and every name filed since the quantifier began, read off the
  level's capture trail (the fold marked every name an iteration's level held). `filed_keys`
  decides "can file": a subrule call's capture name, the rule's own name for an alias that keeps
  it, a silent call's action marker, a token's `$<x>=` aliases.
- A **`~` goal match** matched its inner pattern and its goal in levels of their own and merged
  them, the goal's first (`GoalEnd`). When the goal files no positional capture or marker and no
  name the inner pattern can file, both sides match in place.

Both conditions hold for the grammar shapes: `<pair>* % ','` and `'[' ~ ']' <list>`, including a
`rule`'s implicit `<.ws>`, which is a silent call and so counted as capturing by the coarser test
used before.

The positional axis keeps its run-time shaping (`fold_quantified`, nil reservation, alternation
padding), shared with the walk through ADR-0135 D4's helpers. Deriving it from the pattern, as
Rakudo's capnames analysis does and as the named sets above already are, means re-deriving the
walk's quirks (padding suppressed inside a quantified alternation, `$N=` renumbering) in a second
definition that D6 would have to hold in lockstep. Once ADR-0135 Slice E deletes the walk there is
one definition, and a static positional shape replaces the run-time helpers outright; that is the
follow-up, not part of this decision. Grammars rarely number captures, so it is not where the
allocations are.

### D4. Call frames live in an arena

A compiled call frame is an entry of `Scratch::frames`, linked to its caller by index, instead of
an `Rc`. The arena is cut back to the length a choice point recorded when it resumes, and to a
returning frame's own index when it leaves no choice point in its callee (everything it called
returned the same way). A choice point records its frame state whenever a returned frame is still
in the arena: a callee with no registers opens an empty window, so the register arena's length
alone does not show that a frame is live. A proto call ranks its candidates into scratch buffers
and copies the ranking only when a choice point needs the rest of it.

The same reuse applies to the run's other per-call storage: a level that closes for good hands its
undo trail to the next level opened, and an LTM measurement of a proto candidate (#10487's
territory, which keeps its own goal) reuses its run's vectors.

### D5. A capture's `:sym` and alias rule name are interned and inline

`RegexCaptures` packs both into one word (`NodeNames`, keeping the accumulator within its 128-byte
budget), and `CapNode` holds them as `Option<Symbol>`.

## 3. Consequences

- **Gain**: the allocations removed are structural, not shaved: no delta exists on the compiled
  engine's return path, no per-iteration level exists for the grammar shape of a separated
  quantifier, and no name set is computed at run time. The compiled engine's capture handling is
  simpler by the same measure (`SepNames` replaces `Collect`/`SepEmit` for that shape).
- **Risk**: the two engines now differ in *how* they reach a separated quantifier's or a goal
  match's captures (the walk still folds levels). D6 compares the resulting trees on every file of `t/` and the
  roast whitelist, and `t/grammar/grammar-separated-quantifier-named-captures.t` pins the rakudo
  values. One difference is deliberate and unobservable: a `<x=.y>` alias inside such an atom now
  reaches the level's capture alias map, which the fold dropped; nothing reads that map's attribute.
- The walk keeps building deltas; it is deleted in ADR-0135 Slice E, so making it allocate less
  would be discarded work (ADR-0135 §7).

## 4. Rejected alternatives

- **Keep the hash map and shave around it** (pre-sized tables, a pooled delta). It keeps the
  delta-and-merge interface that is the cause, and every shaved layer is one more cache to keep
  correct.
- **Derive the whole capture shape statically now** (§D3's follow-up). Correct architecture, wrong
  order: while the walk exists it is a second definition of positional shaping.
- **One arena per parse for the Match tree** (a node is an index into a shared tree, not an
  `Arc`). It would remove the per-node allocation too, but every `CapNode` consumer (the lazy
  `Match`, the reduce walk, the action walk, D6) changes with it, a small sub-Match would keep the
  whole parse alive, and §5 shows the goal does not need it.

## 5. Implementation status

Measured with `scripts/bench-det.sh benchmarks/bench-grammar-parse-big.raku` (debug build; the
debug and release counts agree to within 30 allocations on `main`: 192,865 and 192,837):

| step | allocations |
|---|---:|
| `main` (0ece0536) | 192,865 |
| D1 named axis | 154,782 |
| D5 inline `:sym` / alias name | 146,461 |
| D2 subrule returns file in place | 138,136 |
| D4 frame arena, proto ranking in scratch | 125,336 |
| D3 static name sets, separated quantifiers in place (named-only separators) | 120,876 |
| D3 goal matches and sigspace separators in place; trail and LTM vector reuse | 60,366 |
