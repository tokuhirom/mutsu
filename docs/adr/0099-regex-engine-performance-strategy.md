# ADR-0099: Regex engine performance — fix the ceremony first; a prefilter and a fast lane above the unchanged walk

- **Status**: Proposed (2026-09-13; revised the same day after review — see §8)
- **Context**: the eight regex benchmarks added on 2026-09-12/13 (`bench-regex-{match,capture,global,assertion,long-subject,split-subst}.raku`,
  `bench-grammar-parse-big.raku`, `bench-yaml-parse-big.raku`) gave the suite its first view of the
  regex engine. The question this ADR answers: is micro-benchmark-driven tuning of the present
  engine still the right mode of work, or has the architecture become the ceiling?
- **Relates to**: [ADR-0007](0007-grammar-parse-trail-matcher.md) (cursor + trail), [ADR-0016](0016-span-based-captures-and-lazy-match.md)
  (span captures, lazy `Match`), [ADR-0073](0073-regex-atom-candidates-are-demand-driven.md) (demand-driven candidates),
  [ADR-0022](0022-regex-alternation-ltm-ranking.md) (LTM), [ADR-0046](0046-proto-token-ltm-shares-one-ranking-mechanism.md)
  (proto-token LTM), [ADR-0009](0009-regex-code-assertion-execution-model.md) (code assertions).
  ADR-0007 named "full CPS→bytecode regex VM" as *the eventual ceiling*; this ADR is where that is decided.

## 1. What the engine is today

`RegexPattern { tokens: Vec<RegexToken> }` is an **AST**, and matching is a recursive
depth-first walk over it (`regex_walk_ends_in_pkg` → `walk_tokens` → `for_each_atom_candidate` →
`regex_match_atom_*`), with a single mutable `CapStore` plus an undo trail. Three campaigns have
already removed the obvious costs from this shape: by-value capture threading (ADR-0007), stored
capture text and eager `Match` construction (ADR-0016), eager candidate materialization
(ADR-0073), and the field-layout/hashing work of [#7576](https://github.com/tokuhirom/mutsu/issues/7576).

What has **never** existed in it:

- **No prefilter.** There is no memoized literal-prefix extraction, no first-character set, no
  minimum-length bound. Every unanchored scan is `for start in 0..=chars.len()` with a full engine
  entry at each position (`regex_match_find.rs`; `regex_match_captures_impl` even materializes that
  range into a `Vec` first). Note the *ingredients* exist: `ltm_litlen_at`
  (`regex_ltm_rank.rs:221`) is already ADR-0022 §4.3's declarative-leading-literal construction
  table — but it is a per-position matcher-time walk used only to break LTM ties, not a memoized
  static fact about a pattern.
- **No compiled form.** The walk re-interprets the token tree, re-enters five layers of candidate
  generators, and constructs a fresh `RegexCaptures` accumulator *per start position*.

## 2. Measurements (release, this box, 2026-09-13; rakudo 2026.07)

**Methodology note, and the correction that reorganized this ADR.** The first draft compared
whole-process wall clock net of *startup* (mutsu 6 ms, raku 181 ms). That is not enough: rakudo
needs several iterations of a hot loop before it settles, so a short benchmark charges rakudo its
**warm-up** as well. Every figure below is either steady-state (median of late iterations of an
in-process loop) or a marginal cost between two iteration counts. Where the two differ, both are
shown, because the difference is itself the finding.

### 2.1 On the suite as it stands, mutsu wins — but by much less than one-shot timing suggests

One-shot, net of startup:

| bench | mutsu | raku | mutsu is |
|---|---:|---:|---:|
| regex-match | 201 ms | 847 ms | 4.2x faster |
| regex-capture | 266 ms | 916 ms | 3.4x faster |
| regex-assertion | 201 ms | 477 ms | 2.4x faster |
| regex-split-subst | 267 ms | 583 ms | 2.2x faster |
| regex-global | 179 ms | 299 ms | 1.7x faster |
| regex-long-subject | 300 ms | 519 ms | 1.7x faster |
| grammar-parse-big | 153 ms | 398 ms | 2.6x faster |

`bench-yaml-parse-big` is absent because it has **no rakudo baseline on this box** (YAMLish is not
installed for the system rakudo), so no claim of the form "mutsu wins all eight" is available.

Warm marginal cost tells a different story. Running `bench-regex-match` at 20 and at 200 outer
iterations: mutsu 207 → 1,885 ms, raku 1,028 → 3,906 ms, i.e. **9.3 vs 16.0 ms per iteration —
1.7x, not 4.2x**. Most of the headline gap is rakudo's warm-up, which a benchmark this short
charges it in full.

### 2.2 Grammar parsing: mutsu is ~1.9x SLOWER than warm rakudo

Steady-state (30 in-process iterations, median of the last five), on the suite's own
`bench-grammar-parse-big` grammar, 5,173-char document:

| | µs/char |
|---|---:|
| mutsu | **14.4** (flat from iteration 1) |
| rakudo | **7.7** (42.8 on iteration 1, settled by ~iteration 3) |

ADR-0007's "~30x slower than rakudo on grammar parses" and PLAN §4's "~25x per matched character"
**are stale and must be retired** — that campaign measured 18 **ms**/char, three orders of
magnitude worse than today. But the correct replacement is "still ~2x slower than warm rakudo",
not a win. An earlier draft of this ADR claimed 2.1x *faster*; that was measured on a simplified
grammar written for the purpose which omitted the `<sym>`-bodied proto variants, over five
iterations that averaged in rakudo's warm-up. Both errors flattered mutsu. §2.3 is why the grammar
matters.

### 2.3 The whole grammar gap is subrule *resolution*, not matching — and `<sym>` is the trigger

Three-way A/B on the suite grammar, identical document, steady state:

| grammar variant | mutsu | rakudo |
|---|---:|---:|
| as shipped (proto + three `<sym>` bodies) | 14.4 µs/char | 8.2 µs/char |
| same proto, `<sym>` bodies spelled as their literals (`{ 'true' }`) | **4.4 µs/char** | 8.0 µs/char |
| proto replaced by one plain alternation token | 5.0 µs/char | 6.9 µs/char |

So it is not the proto — it is **`<sym>`, and it costs 3.3x on the whole parse**. Rakudo is
unaffected by the same change. With `<sym>` spelled out, mutsu goes from ~1.8x slower than warm
rakudo to ~1.8x faster.

Callgrind on a 160-pair parse (635 M instructions) says where the time goes, by inclusive cost:

| | inclusive | calls |
|---|---:|---:|
| `parsed_subrule_candidates` | **72.9%** | 16,009 |
| `collect_token_patterns_for_scope_dedup` | 47.3% | 29,246 — one `HashMap` clone each (`regex_resolve.rs:273`) |
| `collect_token_patterns_for_scope` → `instantiate_token_pattern` | 33.4% / 17.8% | 29,176 — `format!` + `String::clone` per call |
| `parse_regex` | 18.9% | 16,674 |
| `parse_regex_uncached` (real re-parses) | 8.4% | **6,263 per parse** |
| `regex_pattern_is_static` | 9.1% | `pattern.chars().collect()` per call (`regex_parse.rs:236`) |

Every matcher function combined (`walk_tokens`, `for_each_atom_candidate`, the atom matchers,
`merge_delta`) is under ~12% self-cost; `str::find` over *pattern source text* is 13%.

Mechanism, confirmed at a breakpoint on the declining line: `<sym>` lowers to the pattern source
`:ratchet $<sym>=[true]`, and `regex_pattern_is_static` (`regex_parse.rs:232`) treats `$<` as a
variable form, so it calls that candidate dynamic — though a `$<name>=` capture binding depends on
no runtime value. `resolve_parsed_token_candidates_in_pkg` (`regex_token_resolve.rs:87`) then
declines the memo **for the whole proto** because one candidate is non-static, and the caller
re-resolves and re-parses all seven variants on every `<value>` call. `JSON::Tiny::Grammar` has
exactly this shape.

This is the single largest item in this ADR, it is a **caching defect rather than an architectural
one**, and neither of the layers proposed below touches it: a scan prefilter buys an anchored
`.parse` nothing, and a fast lane optimizes the walk *inside* a token. ADR-0007's implementation
outcome already flagged "a still-unexplained runtime regex re-parse path (`parse_regex_uncached`
+ LTM expansion ~4%)"; it is now ~70%.

### 2.4 Scanning: an asymptotic loss, not a constant one

Failing scans over a repeated 46-char unit, milliseconds:

| subject | mutsu literal | raku literal | mutsu alternation | raku | mutsu `\w+` greedy | raku | mutsu `:i` literal | raku |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 10 KB | 1 | 1 | 10 | 12 | 6 | 4 | 2 | 0 |
| 40 KB | 4 | **0** | 38 | 15 | 19 | 14 | 10 | **0** |
| 160 KB | 15 | **0** | 154 | 86 | 77 | 56 | 39 | 2 |
| 640 KB | 65 | **0** | 612 | 355 | 314 | 228 | 158 | 6 |

The literal columns are not constant-factor gaps but **asymptotic** ones: rakudo lowers a
literal-only pattern (case-folded included) to a substring search and stays flat through 640 KB,
while mutsu walks every position. On the columns where both are linear, mutsu's constant is
1.6–1.7x worse. The `:i` column matters for the design below: any prefilter that does not cover
case-folded prefixes leaves a 26x gap standing.

Callgrind on a 65,536-position failing literal scan: **~983 instructions per start position** (net
of the 9.0 M instructions of `mutsu -e 'say 1'`) to establish that `chars[i] != 'z'`. The profile
is the machinery, not the comparison: the five candidate-generator layers, candidate-`Vec`
construction and drop, `memcpy`, `malloc`, and `RegexCaptures` construction/drop.

### 2.5 Small-subject matching is ceremony-bound, and mutsu loses it to warm rakudo

`"a" ~~ /a/` in a 200k-iteration loop: **4.9 µs/call warm against rakudo's 1.78 µs — mutsu is 2.8x
slower** on the shape that is most of real Raku code. 35.6k instructions per call, net of startup.
Inclusive shares:

| | share |
|---|---:|
| `regex_match_with_captures_value` (everything regex) | 20.5% |
| — of which the matcher proper | **8.2%** |
| `Symbol::intern` (40,000 calls — two per match) | **16.4%** |
| `eval_truthy` | **14.4%** (`resolve_method_with_owner_impl` 10.3% under it) |
| `match_attr` / `MatchNode::attr` | 14.6% (`materialize_map` 9.5%) |
| `set_env_with_main_alias_inner` (`$/` publication) | 4.3% |

Two ceremony costs dominate the matcher:

- **`eval_truthy`** — turning the Match into the Bool the `if` wanted — runs the full user-method
  overload lookup and forces `MatchNode::materialize_map` through a `Once::call_once_force`,
  20,000 times for 20,000 matches. ADR-0016's lazy `Match` is defeated by its cheapest consumer.
- **String-keyed `Env` traffic**: `smart_match.rs:961-968` saves and restores `$_` with
  `env.get("_")` / `env.insert("_".to_string(), …)` / `remove("_")`, plus `remove("made")` and
  `insert(i.to_string())` per positional capture — two `Symbol::intern`s per match at ~2,450
  instructions each. `src/symbol.rs:762` already has a pre-interned table for exactly this kind of
  hot fixed name.

So for a small subject the regex engine is **8% of the cost of a regex match**. Making the matcher
infinitely fast would win 8%, and would not close the 2.8x.

### 2.6 On a realistic subject the balance inverts

`bench-regex-match.raku` (~110-char lines, 8 patterns): the matcher is **61% inclusive**. Within
the whole run, allocator traffic is ~16%, candidate-`Vec` `from_iter` + `IntoIter::drop` ~4%,
`LocalKey::with` ~4% — and `parse_regex_uncached` is **4.5%**, because the file's two interpolated
patterns (`$needle \d+`, `<$rx>`) fail `regex_pattern_is_static` and so bypass `REGEX_PARSE_CACHE`
entirely, re-parsing on every iteration.

### 2.7 `:ignoremark` is O(n²)

`regex_match_ends_from_caps_in_pkg_impl` (`regex_match_core.rs:255-283`) handles a pattern with
`ignore_mark` set by calling `strip_marks_text(&chars[start..])` **and** `strip_marks_pattern` on
every invocation — it rebuilds the mark-stripped image of the whole remaining subject each time
the atom is entered. `strip_marks_text` (`regex_helpers.rs:503`) collects into a `String` and walks
graphemes + NFD, and `drive_subpattern_candidates` declines ADR-0073 streaming for `:m`
(`regex_match_lazy.rs:171`).

```raku
grammar A { token TOP { [ <s> \s* ]+ }  token s { (:ignoremark '"') ~ '"' ( <-["]>* ) } }
grammar C { token TOP { [ <s> \s* ]+ }  token s { (:ignorecase '"') ~ '"' ( <-["]>* ) } }
grammar B { token TOP { [ <s> \s* ]+ }  token s { ('"')             ~ '"' ( <-["]>* ) } }
```

| chars | `:m` | `:i` | plain | rakudo `:m` |
|---:|---:|---:|---:|---:|
| 219 | 2 ms | 0 ms | 0 ms | 3 ms |
| 879 | 6 ms | 1 ms | 1 ms | 2 ms |
| 3,519 | 73 ms | 3 ms | 2 ms | 7 ms |
| 14,079 | **1,120 ms** | 10 ms | 9 ms | 17 ms |

4x the input costs `:m` 15x the time; `:i` and plain are linear, and so is rakudo. **`:i` does not
share this mechanism** (`regex_casefold.rs` does not re-strip per atom) — an earlier draft assumed
it did.

`JSON::Tiny::Grammar` has the `(:ignoremark '"')` shape at `JSON/Tiny/Grammar.pm:27`, so its
parse-only curve is superlinear: 3 / 7 / 60 / 767 ms at 156 / 638 / 2,606 / 10,792 chars, against
rakudo's 5 / 5 / 7 / 27.

One design constraint the fix must respect: the `:m` here is **scoped** (`(:ignoremark '"')`), so
"strip the subject once per engine entry" taken literally would let the rest of the pattern see
stripped text. The fix is a *cached stripped view* plus both-direction position maps on the
`MatchTarget`, consumed only by the scoped sub-pattern.

Two adjacent defects of the same family:

- `parse_regex` builds its cache key with `format!("{}\u{0}{}", package, pattern)`, and
  `regex_pattern_is_static` in the same function collects the pattern into a `Vec<char>` — both
  per match call.
- A pattern that fails `regex_pattern_is_static` bypasses `REGEX_PARSE_CACHE` altogether (§2.6),
  so any interpolated pattern in a loop is re-parsed every iteration.

## 3. The question, restated honestly

"Keep micro-benchmarking" and "re-architect" are not the real alternatives, because the evidence
splits four ways and only one part is about the matcher:

1. **Grammar parsing** is **resolution-ceremony-bound** (§2.3) and mutsu loses it ~1.9x. A caching
   defect, not an architecture.
2. **Small-subject matching** is **ceremony-bound** (§2.5) and mutsu loses it ~2.8x. Method
   resolution, lazy-Match forcing, and string-keyed `Env` traffic — none of it the engine's.
3. **Scanning** is **prefilter-bound** (§2.4). An asymptotic loss that tuning the walk cannot close.
4. **`:ignoremark`** carries an outright O(n²) (§2.7).

Micro-benchmarking found none of the four, and could not have: (1) and (2) are invisible because
the benchmarks measure regex against regex rather than against the plumbing, and because one-shot
timing pays rakudo's warm-up for it; (3) is invisible because the benchmarks are sized below the
crossover; (4) is invisible because O(n²) with a small constant looks linear at 1 KB. That is the
verdict on the measurement regime, and it is separate from the verdict on the engine.

The verdict on the engine is the harder one, and it is this: **the walk is not where mutsu is
losing.** It is ~8% of a small match (§2.5), under ~12% of a grammar parse (§2.3), and the two
places it does dominate are a missing prefilter (§2.4) and a benchmark of its own shape (§2.6).

## 4. Decision

**Do not rewrite the engine. Remove the ceremony that the measurements actually indict, add a
prefilter layer above the unchanged walk, and defer everything that touches the walk itself until
grammars have been re-profiled without the ceremony.**

### Stage 0 — the measured losses (this is the substance of the ADR, not preliminaries)

Ordered by measured size:

1. **`regex_pattern_is_static` stops misreading `$<name>=` as interpolation, and the proto memo is
   declined per candidate rather than wholesale** (§2.3, ~70% of a grammar parse; 3.3x on the suite
   grammar). The follow-up, worth measuring after those two: a resolved token table built once per
   parse instead of per subrule reference, which is the rakudo model.
   [#8265](https://github.com/tokuhirom/mutsu/issues/8265).
2. **`:ignoremark` strips once, into a cached view on the `MatchTarget`** (§2.7), preserving scoped
   `:m`. Memoize `strip_marks_pattern` next to the parsed pattern.
   [#8262](https://github.com/tokuhirom/mutsu/issues/8262).
3. **A `Match` answers `Bool` from its own span** — no user-method lookup, no `materialize_map`
   (§2.5, 14.4%). [#8263](https://github.com/tokuhirom/mutsu/issues/8263).
4. **The smartmatch path stops interning `$_` / `made` / `$0…$N` per match** (§2.5, 16.4%) —
   pre-interned symbols and symbol-keyed `Env` access, per `src/symbol.rs:762`; and stops cloning
   the subject into `$_`. [#8269](https://github.com/tokuhirom/mutsu/issues/8269).
5. **Parse-cache repairs** (§2.6, §2.7): a non-allocating cache key, `regex_pattern_is_static`
   without a per-call `Vec<char>`, and a second cache level keyed on the interpolated pattern so a
   runtime-interpolated pattern in a loop parses once rather than per iteration.
   [#8270](https://github.com/tokuhirom/mutsu/issues/8270).

Every one of these is a defect or waste with no design risk, and together they address both
measured losses. **They gate everything below**: Stage 1's value cannot be assessed against a
grammar profile that is 70% resolution ceremony.

### Stage 1 — a memoized static **prefilter**, specified as ADR-0022's table

One pass over a parsed `RegexPattern`, memoized alongside it in `REGEX_PARSE_CACHE`, deriving:
required literal prefix (→ substring search), first-character set (→ reject a start position in ~1
instruction instead of ~983), minimum match length, and a required inner literal for patterns with
no usable prefix.

Three constraints that are Raku-specific and non-negotiable:

- **It is the memoized static form of `ltm_litlen_at`'s construction table (ADR-0022 §4.3), not a
  second definition.** Two definitions of "declarative literal prefix" in one codebase will drift,
  and the drift is silent (a dropped valid match).
- **`:i` needs fold-closure first-sets, not a folded needle.** Multi-character folds (`ß`/`SS`,
  `ﬁ`/`fi`) make a folded literal prefix variable-length, so `memmem` over it is unsound; `:m`
  needs the first-set to be NFD-aware on the subject side.
- **Decline on anything non-declarative.** A leading `{ }` block runs once per start position in
  both mutsu and rakudo (ADR-0009); a prefix derived through `<subrule>` must be keyed by invocant
  package and `TOKEN_DEFS_GEN` (dynamic override via `H is G` is legal), or decline.

Scope, stated plainly so it is not over-sold: **this is a scan optimization.** It does nothing for
`Grammar.parse` or for any anchored or subrule-entered match. It is load-bearing for §2.4's
workload and for the `:g` / `.comb` / `.subst` / `split` scan loops, and for nothing else.

### Stage 2 — deferred, not decided

A fast lane compiling the regular subset of Raku regex to a flat instruction program, interleaving
with the general engine at subrule boundaries (ADR-0073's `MatchSink::Cont` already provides the
boundary), was the first draft's Stage 2. It is **not decided here.** §2.3 says the walk is under
~12% of a grammar parse today, and the remaining per-subrule ceremony — resolution,
`build_named_candidates_from_inner`, `CapNode` construction, action dispatch — sits outside any
fast lane. **Re-profile grammars after Stage 0 and decide in a superseding ADR**, with the
question being whether the walk has become the majority cost once the ceremony is gone.

### Stage 3 — the CPS→bytecode regex VM stays deferred

ADR-0007's eventual ceiling. Behind Stage 2, behind a measured trigger, and needing its own
superseding ADR.

## 5. Rejected alternatives

- **Keep micro-optimizing the walk only.** Rejected on gain: §2.4's literal columns are asymptotic
  and cannot be reached from inside the walk, and §2.3/§2.5 show that both of the places mutsu
  actually loses are outside it. Another round of allocation tuning would move §2.6's ~16%
  allocator share and nothing that is measured as a loss.
- **A big-bang rewrite to a compiled regex VM, now.** Rejected on **gain, not on risk**: it does not
  move a single measured loss. §2.5 gives it ~8% of a small match, §2.3 gives it under ~12% of a
  grammar parse, and §2.7's quadratic and §2.4's missing prefilter are both untouched by it. (The
  first draft argued this on blast radius and "no intermediate shippable state" — that is precisely
  the vocabulary CLAUDE.md's gain/risk definitions rule out, and it was the wrong argument even
  though the conclusion holds.)
- **Replace the engine with the `regex` crate, or compile the whole language to a DFA.** Rejected
  on semantics. Raku regex is not a regular language: `<?{ }>`/`<{ }>` run interpreter code
  mid-match (ADR-0009), LTM ranks declaratively over the prefix (ADR-0022/0046), protoregex
  dispatch, left recursion, `~` goal matching, backreferences and `:my` lexicals are all outside it.
- **An NFA over the declarative prefix — NOT rejected, folded into Stage 1.** Rakudo uses an NFA for
  exactly this (LTM ranking plus start filtering), and mutsu's LTM is a per-branch measurement walk
  at every position, which is §2.4's alternation column. Stage 1's first-set/prefix analysis is the
  same construction; whether it is realized as an NFA is an implementation choice inside Stage 1,
  and a natural one.
- **Skipping `$/` publication in boolean context.** Rejected: Raku sets `$/` on `~~` regardless of
  how the result is consumed. Stage 0 item 3 gets the measured win without the semantic change.

## 6. What the benchmark suite should become

The eight files stay — they are the first real coverage the engine has had. Three changes follow
from §3:

- **Report warm marginal cost, not one-shot wall clock.** §2.1's 4.2x and §2.2's inverted result
  were both artifacts of charging rakudo its warm-up. A benchmark that cannot see rakudo warm
  cannot steer work against rakudo.
- **Sizes must straddle the crossover.** `bench-regex-long-subject.raku` is the only file that looks
  past per-call setup, and it is deliberately sized *below* the point where mutsu loses. Record the
  ratio at two sizes, or size it past the crossover and accept an unflattering row.
- **Keep a real module's grammar in the suite, and give the yaml file a baseline.**
  `bench-grammar-parse-big.raku` uses a hand-written grammar; `JSON::Tiny::Grammar` is superlinear
  on the same shape of document (§2.7). `bench-yaml-parse-big.raku` already records an ~n^1.25 curve
  but has no rakudo baseline on this box, which is what kept it out of §2.1.

Neither is an argument against micro-benchmarks. It is an argument that a benchmark suite steers
only the costs it is shaped to see, and this one was shaped around one-shot cost on short subjects.

## 7. Consequences

- ADR-0007's "~30x slower than rakudo on grammar parses" and PLAN §4's "~25x per matched character"
  are retired as stale — but the replacement is **~1.9x slower warm** (§2.2), not a win, and §2.3
  says nearly all of it is one caching defect.
- [#8183](https://github.com/tokuhirom/mutsu/issues/8183) (the JSON `use`-time interception) was
  **closed on 2026-09-13** by PR #8203, and `docs/batteries/json-tiny.md` already corrects the "600 s
  for 200 META documents" figure to **12.6 s** (raku 0.84 s), noting zef's metadata path was never
  on it. §2.7's quadratic and §2.3's resolution defect are both on the remaining 15x, and both
  should be re-measured against that 12.6 s after Stage 0 — but no rung-3 exception is waiting on
  this ADR.
- ADR-0007's "eventual ceiling" is confirmed as real but re-ordered behind Stage 0 and Stage 1, and
  Stage 2 is demoted from a decision to a question.
- Stage 1 introduces the first piece of mutsu regex machinery that can be **wrong without being
  incorrect** — an over-promising analysis silently skips a valid match. Every derived fact needs a
  differential property test against the unfiltered walk, not only a roast pass. Building it on
  ADR-0022's existing table rather than a fresh heuristic is the primary mitigation.

## 8. Implementation status

Nothing implemented. Stage 0 is fully filed, ordered as in §4:

| item | issue | measured cost |
|---|---|---|
| 1. `<sym>` kills the proto-candidate memo | [#8265](https://github.com/tokuhirom/mutsu/issues/8265) | 3.3x on a grammar parse |
| 2. `:ignoremark` re-strips per invocation | [#8262](https://github.com/tokuhirom/mutsu/issues/8262) | O(n²) |
| 3. Match truthiness forces `materialize_map` | [#8263](https://github.com/tokuhirom/mutsu/issues/8263) | 14.4% of a boolean `~~` |
| 4. smartmatch interns `$_` twice per match | [#8269](https://github.com/tokuhirom/mutsu/issues/8269) | 16.4% of a boolean `~~` |
| 5. parse-cache key, scan, and interpolated bypass | [#8270](https://github.com/tokuhirom/mutsu/issues/8270) | 4.5% of `bench-regex-match` |

Stage 1 is deliberately **not** filed while this ADR is `Proposed`: it is the one part of the plan
that is a design commitment rather than a defect, and filing an implementation ticket for an
unaccepted decision would put it in the `todo:ticket` queue for an agent to pick up. File it as
`todo:deep` when this ADR moves to `Accepted`. Stage 2 is a question, not work, until Stage 0
lands and grammars are re-profiled.

Revision history: first draft 2026-09-13, reviewed the same day. The review inverted §2.2 (the
grammar claim had been measured on a simplified grammar over five iterations including rakudo's
warm-up), produced §2.3 (which the first draft missed entirely and which is now the largest item; the review
attributed it to the proto mechanism generally, and a follow-up A/B plus a breakpoint narrowed it to
`<sym>` and named the exact misclassification),
corrected §2.5's shares and added the `Symbol::intern` finding, established that `:i` does not share
§2.7's mechanism, found #8183 already closed, withdrew a misattribution of `regex_find_first` to the
`~~` path (its callers are `.contains`/`.grep`/`.first`/hash-key smartmatch, not `~~`), and rewrote
§5's rejection of the big-bang rewrite onto gain grounds. The staging survived; its contents and its
centre of gravity did not.
