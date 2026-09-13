# ADR-0099: Regex engine performance — a prefilter/fast-lane layering, not a big-bang rewrite

- **Status**: Proposed (2026-09-13)
- **Context**: the eight regex benchmarks added on 2026-09-12/13 (`bench-regex-{match,capture,global,assertion,long-subject,split-subst}.raku`,
  `bench-grammar-parse-big.raku`, `bench-yaml-parse-big.raku`) gave the suite its first view of the
  regex engine. The question this ADR answers: is micro-benchmark-driven tuning of the present
  engine still the right mode of work, or has the architecture become the ceiling?
- **Relates to**: [ADR-0007](0007-grammar-parse-trail-matcher.md) (cursor + trail), [ADR-0016](0016-span-based-captures-and-lazy-match.md)
  (span captures, lazy `Match`), [ADR-0073](0073-regex-atom-candidates-are-demand-driven.md) (demand-driven candidates),
  [ADR-0022](0022-regex-alternation-ltm-ranking.md) (LTM), [ADR-0009](0009-regex-code-assertion-execution-model.md) (code assertions).
  ADR-0007 named "full CPS→bytecode regex VM" as *the eventual ceiling*; this ADR is where that is decided.

## 1. What the engine is today

`RegexPattern { tokens: Vec<RegexToken> }` is an **AST**, and matching is a recursive
depth-first walk over it (`regex_walk_ends_in_pkg` → `walk_tokens` → `for_each_atom_candidate` →
`regex_match_atom_*`), with a single mutable `CapStore` plus an undo trail. Three campaigns have
already removed the obvious costs from this shape: by-value capture threading (ADR-0007), stored
capture text and eager `Match` construction (ADR-0016), eager candidate materialization
(ADR-0073), and the field-layout/hashing work of [#7576](https://github.com/tokuhirom/mutsu/issues/7576).

What has **never** existed in it, and is the subject of this ADR:

- **No pattern analysis and no prefilter.** There is no literal-prefix extraction, no first-character
  set, no minimum-length bound. `grep -i 'prefilter\|memchr\|literal_prefix\|first_char'` over
  `src/runtime/regex*` returns nothing. Every scan is `for start in 0..=chars.len()` with a full
  engine entry at each position (`regex_match_find.rs`, `regex_match_captures_impl` even
  materializes that range into a `Vec` first).
- **No compiled form.** The walk re-interprets the token tree, re-enters five layers of candidate
  generators, and constructs a fresh `RegexCaptures` accumulator *per start position*.

## 2. Measurements (release, this box, 2026-09-13; rakudo 2026.07)

### 2.1 The benchmarks as they stand — mutsu wins all of them

Net of startup (mutsu 6 ms, raku 181 ms):

| bench | mutsu | raku | mutsu is |
|---|---:|---:|---:|
| regex-match | 283 ms | 1128 ms | 4.0x faster |
| regex-capture | 266 ms | 916 ms | 3.4x faster |
| regex-assertion | 201 ms | 477 ms | 2.4x faster |
| regex-split-subst | 267 ms | 583 ms | 2.2x faster |
| regex-global | 179 ms | 299 ms | 1.7x faster |
| regex-long-subject | 300 ms | 519 ms | 1.7x faster |
| grammar-parse-big | 153 ms | 398 ms | 2.6x faster |

Steady-state grammar parse (5 iterations of a 7.5 KB document, warm) is **2.1x faster** than
rakudo — 2.9 µs/char vs 6.2 µs/char. ADR-0007's headline "~25-30x slower than rakudo on grammar
parses" and PLAN §4's restatement of it are **stale and should be retired**.

### 2.2 …and every one of those wins evaporates with scale

Failing scans over a repeated 46-char unit, milliseconds:

| subject | mutsu literal | raku literal | mutsu alternation | raku alternation | mutsu `\w+` greedy | raku greedy |
|---:|---:|---:|---:|---:|---:|---:|
| 10 KB | 1 | 1 | 9 | 7 | 6 | 4 |
| 40 KB | 4 | **0** | 37 | 15 | 20 | 14 |
| 160 KB | 15 | **0** | 149 | 103 | 84 | 57 |
| 640 KB | 59 | **0** | 593 | 314 | 344 | 224 |

The literal column is not a constant-factor gap, it is an **asymptotic** one: rakudo lowers a
literal-only pattern to a substring search and stays at 0 ms through 640 KB, while mutsu walks
every position. On the two columns where both are linear, mutsu's constant is 1.5–1.9x worse.
`bench-regex-long-subject.raku`'s own header already records the crossover at ~0.5 MB; the table
above is the same finding on three pattern shapes.

### 2.3 The per-position constant

Callgrind, 65,536-position failing literal scan (`/'zzzq-not-here'/` over 64 KB): **73.1 M
instructions, ~1,100 instructions per start position** — to establish that `chars[i] != 'z'`.
The profile is the machinery, not the comparison: the five candidate-generator layers
(`regex_match_atom_all_with_capture_in_pkg_inner` 9.6%, `regex_walk_ends_in_pkg` 9.1%,
`regex_match_atom_with_capture_in_pkg_inner` 9.0%, `regex_match_atom_in_pkg_inner` 8.9%,
`walk_tokens` 8.0%, `for_each_atom_candidate` 7.8%), plus candidate-`Vec` construction and drop,
`memcpy`, `malloc`, and `RegexCaptures` construction/drop.

### 2.4 The per-call constant, and what it is actually made of

`"a" ~~ /a/` in a 200k-iteration loop: **29,400 instructions per call** (5.8 µs; rakudo 2.0 µs).
Inclusive shares of that call:

| | share |
|---|---:|
| `regex_match_with_captures_value` (everything regex) | 19.2% |
| — of which the matcher proper (`regex_match_with_parsed_captures`) | **7.5%** |
| `match_attr` / `MatchNode::attr` / `materialize_map` | 13.3% |
| `set_env_with_main_alias_inner` (`$/` publication) | 7.0% |
| remainder: smartmatch dispatch, `$_` save/restore, subject `String` clone, `Symbol::intern` (2.9%), SipHash (1.8%) | ~60% |

`eval_truthy` alone — *turning the Match into the Bool the `if` wanted* — costs **9.4%**, because it
goes through `resolve_method_with_owner_impl` (20,000 method resolutions for 20,000 matches) and
forces `MatchNode::materialize_map`. The lazy `Match` of ADR-0016 is being materialized by the
boolean test that was supposed to be the cheapest possible consumer.

So for a small subject the regex engine is **8% of the cost of a regex match**. Making the matcher
infinitely fast would win 8%.

### 2.5 On a realistic subject the balance inverts

`bench-regex-match.raku` (~110-char lines, 8 patterns): the matcher is **70% inclusive**. Within
the whole run, allocator traffic (`malloc`/`free`/`realloc`/`memcpy`) is **~16%**, candidate-`Vec`
`from_iter` + `IntoIter::drop` ~4%, and `LocalKey::with` (thread-local bookkeeping) ~4%.

### 2.6 A real module's grammar is superlinear — an O(n²) defect, not a constant

`JSON::Tiny::Grammar.parse` on a synthetic JSON document, parse only, no actions:

| chars | mutsu | raku |
|---:|---:|---:|
| 221 | 5 ms | 7 ms |
| 911 | 21 ms | 6 ms |
| 3,731 | 197 ms | 20 ms |

16.9x the input costs mutsu 39x the time. Root cause, isolated to the two-grammar repro below:
`token string { (:ignoremark '"') ~ … }`.

```raku
grammar A { token TOP { [ <s> \s* ]+ }  token s { (:ignoremark '"') ~ '"' ( <-["]>* ) } }
grammar B { token TOP { [ <s> \s* ]+ }  token s { ('"')             ~ '"' ( <-["]>* ) } }
for 20, 80, 320 -> $n {
    my $doc = ('"abcdefgh"' xx $n).join(' ');
    my $t = now; A.parse($doc); my $t1 = now; B.parse($doc); my $t2 = now;
    say "n=$n chars={$doc.chars} ignoremark={(($t1-$t)*1000).round}ms plain={(($t2-$t1)*1000).round}ms";
}
# n=20  chars=219   ignoremark=2ms   plain=0ms
# n=80  chars=879   ignoremark=6ms   plain=1ms
# n=320 chars=3519  ignoremark=74ms  plain=3ms
```

`regex_match_ends_from_caps_in_pkg_impl` handles `pattern.ignore_mark` by calling `strip_marks_text(&chars[start..])` — it rebuilds the
mark-stripped image of the **whole remaining subject on every invocation of that atom**. One
`:ignoremark` inside a token that fires once per string in the document makes the parse quadratic.
Grammar `B` — the same token with the `:ignoremark` removed — stays linear.

This is almost certainly the "~600 s to decode 200 META-shaped documents" that
[#8183](https://github.com/tokuhirom/mutsu/issues/8183) records as the reason the native JSON
`to-json`/`from-json` interception still shadows the real module — i.e. a rung-3 exception on
ADR-0096's retirement list is being held open by a bug, not by an architectural deficit.

Two smaller defects of the same family, found while reading the same paths:

- `regex_find_first` does `text.chars().collect()` on **every call** — a fresh `Vec<char>` of the
  whole subject per boolean match. (`regex_find_first_from_with_all_captures_in` was already fixed
  this way in #8247; this sibling was not.)
- `parse_regex` builds its cache key with `format!("{}\u{0}{}", package, pattern)` — a `String`
  allocation plus a SipHash of the full pattern text on every match call.

## 3. The question, restated honestly

"Keep micro-benchmarking" and "re-architect" are not the real alternatives, because the evidence
splits three ways and each part wants a different answer:

1. **Small-subject matching** (most Raku code, and the shape of six of the eight new benchmarks) is
   **ceremony-bound**, not matcher-bound (§2.4). No regex-engine rewrite addresses it.
2. **Scanning** is **prefilter-bound** (§2.2, §2.3). This is an asymptotic loss against rakudo and
   cannot be closed by tuning the walk.
3. **Real grammars** carry at least one **O(n²) defect** (§2.6). No benchmark at benchmark sizes can
   see it, and no constant-factor work can fix it.

Micro-benchmarking found none of these. It could not: (1) is invisible because the benchmarks
measure regex against regex rather than against the plumbing; (2) is invisible because the
benchmarks are sized so that the run takes a few hundred milliseconds today, which is below the
crossover; (3) is invisible because O(n²) with a small constant looks linear at 1 KB. That is the
honest verdict on the measurement regime, and it is separate from the verdict on the engine.

Conversely, the engine has **not** hit an architectural wall in the sense that would justify
throwing it away. It beats rakudo on all eight benchmarks and on steady-state grammar parsing; its
asymptotics are correct except where a specific defect breaks them; and the three prior campaigns
each delivered without a rewrite.

## 4. Decision

**Do not rewrite the engine. Add two layers above and beside it, fix the algorithmic defects
first, and keep the general walk as the semantic authority.** Concretely, in this order:

### Stage 0 — algorithmic defects and pure waste (no architecture change)

- `:ignoremark` strips the subject **once per engine entry**, not once per atom invocation
  (§2.6). Pin with a scaling test that fails on a quadratic curve, not just a correctness test.
- Audit every other per-invocation whole-subject transform on the same pattern (`:i` case folding,
  `strip_marks_pattern`) and hoist them the same way.
- `regex_find_first` takes a `MatchTarget` like its sibling; kill the per-call `chars().collect()`.
- `parse_regex`'s cache key stops allocating (interned package `Symbol` + pattern pointer/`Symbol`,
  or a two-level map keyed by package).
- A Match's truthiness answers from its span without method resolution and without
  `materialize_map` (§2.4) — this alone is ~9% of every boolean `~~`.

These are bugs and waste. They are worth more on real code than anything else in this ADR and they
carry no design risk.

### Stage 1 — a compile-time **analysis + prefilter** layer (the load-bearing change)

One pass over a parsed `RegexPattern`, memoized next to it in `REGEX_PARSE_CACHE`, deriving:

- **required literal prefix** → an unanchored scan becomes a substring search (`memmem`) that jumps
  straight to the next plausible start, instead of entering the engine at every position;
- **first-character set** → a 256-bit ASCII bitmap (plus a fallback predicate for non-ASCII) that
  rejects a start position in ~1 instruction instead of ~1,100;
- **minimum match length** → truncates the start range, and prunes the tail of every scan;
- **required inner literal** (a literal that must appear anywhere in any match) → same skip for
  patterns with no usable prefix.

The engine below is untouched, so the semantic risk is confined to "did the analysis
over-promise" — testable exhaustively, and the roast suite is the net. This is what makes §2.2's
literal column match rakudo and cuts the alternation and greedy columns; it is also what the `:g` /
`.comb` / `.subst` / `split` scan loops inherit for free, since they all resume through the same
per-position walk.

### Stage 2 — a **fast lane** for the regular subset (only after Stage 1 is measured)

Compile the *regular* subset of Raku regex — literals, character classes, `.`, anchors, greedy and
frugal quantifiers, non-capturing alternation, and positional captures — to a flat instruction
program run by a tight loop with an explicit position stack: no per-start `RegexCaptures`, no
candidate `Vec`s, no thread-local lookups. Anything outside the subset (code assertions `<?{ }>`,
closure interpolation `<{ }>`, subrule calls, LTM/protoregex, left recursion, `~` goal matching,
backreferences, `:my` lexicals) falls through to today's engine, unchanged.

The design constraint that makes this worth doing for **grammars** and not only for flat patterns:
the two lanes must **interleave** — a subrule call from the fast lane re-enters the general engine
and vice versa. Most individual tokens in a real grammar are in the regular subset even when the
grammar as a whole is far outside it.

### Stage 3 — absorb the general path into the compiled form

Only on a measured trigger, and only with a superseding ADR. This is ADR-0007's "CPS→bytecode regex
VM". Stages 1 and 2 are its prerequisites, and if they land well, Stage 3 may never pay for itself.

## 5. Rejected alternatives

- **Keep micro-optimizing the walk only.** Rejected: §2.2's literal column is asymptotic, and
  §2.4 shows the small-subject case is not the walk's to win. A fourth round of allocation tuning
  would move §2.5's ~16% allocator share and nothing else.
- **Big-bang rewrite to a compiled regex VM.** Rejected as a *first* move, not on principle:
  §2.4 says it buys ~8% on small subjects, §2.6 says it does not fix the quadratic, and it would put
  every LTM / protoregex / code-assertion / left-recursion semantic — five ADRs' worth, and the
  bulk of 27,500 lines under `src/runtime/regex*` — through a single high-risk change with no
  intermediate shippable state. Stage 2 gets most of the win with a fallback path.
- **Replace the engine with the `regex` crate, or compile to a DFA.** Rejected on semantics. Raku
  regex is not a regular language: `<?{ }>`/`<{ }>` run arbitrary interpreter code mid-match
  (ADR-0009), LTM ranks declaratively over the prefix (ADR-0022), protoregex dispatch, left
  recursion, `~` goal matching, backreferences and `:my` lexicals are all outside it. A DFA is
  admissible only *inside* Stage 2's subset, as an implementation choice of the fast lane.
- **Make `$/` publication lazy/skippable in boolean context.** Rejected as stated: Raku sets `$/`
  on `~~` regardless of how the result is consumed, so it cannot be skipped. Stage 0's Match
  truthiness fix gets the measured win without changing the semantics.

## 6. What the benchmark suite should become

The eight files stay — they are the first real coverage the engine has had, and §2.1 is a genuine
result worth defending. Two changes follow from §3:

- **Sizes must straddle the crossover.** `bench-regex-long-subject.raku` is the only file that
  looks past per-call setup, and it is deliberately sized *below* the point where mutsu loses. A
  scan benchmark whose numbers are only ever flattering cannot steer this work; record the ratio at
  two sizes, or size it past the crossover and accept an unflattering row.
- **A real module's grammar belongs in the suite.** `bench-grammar-parse-big.raku` uses a
  hand-written grammar and is linear; `JSON::Tiny::Grammar` is 39x superlinear on the same shape of
  document (§2.6). The defect lives in the gap between them, and only the second kind of benchmark
  can see it.

Neither is an argument against micro-benchmarks. It is an argument that a benchmark suite steers
only the costs it is shaped to see, and this one was shaped around per-call cost on short subjects.

## 7. Consequences

- PLAN §4's "Grammar/regex per-subrule ceremony (~25x vs raku per matched character)" is superseded
  by §2.1/§2.6: the per-character ceremony is gone (mutsu is 2.1x *faster* than rakudo on a warm
  grammar parse), and what is left of that item is the `:ignoremark` quadratic. #8183's premise
  should be re-measured after Stage 0 before any further rung-3 reasoning is done on it.
- ADR-0007's "eventual ceiling" is confirmed as real but re-ordered behind the prefilter layer.
- Stage 1 introduces the first piece of mutsu regex machinery that can be *wrong without being
  incorrect* (an over-promising analysis silently skips a valid match). Every derived fact needs a
  property test against the unfiltered walk, not only a roast pass.
