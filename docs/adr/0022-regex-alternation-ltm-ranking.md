# ADR-0022: `|` alternation ranks branches by declarative-prefix LTM, not by longest actual match

- **Status**: Accepted (2026-08-09); Slices 1-3 implemented and merged 2026-08-11
  (measurement infrastructure, litlen, and the ranking swap in all three consumer
  arms — `roast/S05-metasyntax/longest-alternative.t` tests 28/54 and Cro::HTTP
  `t/http-router.rakutest` test 61 now pass, pinned by `t/regex-ltm-alternation.t`
  and `t/regex-ltm-declarative-prefix.t`). Slices 4 (ledger update) and 5 (non-constant
  interpolation marking, needed for roast test 50) remain — see §5 and §2.
- **Context**: `todo/deep/regex-alternation-ltm-longest-literal-prefix.md`;
  `Cro::HTTP` `t/http-router.rakutest` test 61 (the file's last remaining failure);
  `roast/S05-metasyntax/longest-alternative.t` tests 28/50/54 (the file's only failures,
  62-test file — the BLOCKERS.md "57/62" row is stale).
  Builds directly on ADR-0009's `LTM_DECLARATIVE_MODE` machinery.
- **Scope**: semantics of `|` branch *ranking* in the regex engine. Protoregex dispatch
  already ranks by declarative prefix (ADR-0009, `dispatch.rs::eval_token_call_values_at`)
  and is NOT changed here; interpolated-`@array` alternation already approximates LTM by
  length-sorting and is not changed here.

## 1. Problem

mutsu's `RegexAtom::Alternation` ranks branches by **longest actual (full-branch) match,
ties broken by declaration order**. Rakudo ranks by **longest declarative-prefix match,
ties broken by longest-literal (litlen), then declaration order**. Three observable
divergence classes:

1. **Tie + litlen** (the Cro bug):
   ```raku
   "/category/tree" ~~ / "/category/" (\w+) | "/category/tree" /;
   say $0.defined ?? "capture-branch" !! "literal-branch";
   # raku: literal-branch    mutsu: capture-branch
   ```
   Both branches match 14 chars; rakudo breaks the tie toward the branch whose match
   crossed more *leading literal* characters (14 vs 10).
2. **Prefix, not full length** (roast test 28): a branch whose *declarative prefix* is
   shorter must lose even when its *full* match is longer.
   ```raku
   my rule  ltm_ws1 {\w+ '-'+}    # implicit <.ws> ends the prefix after \w+ (len 3)
   my token ltm_ws2 {\w+ '-'}     # fully declarative (len 4)
   'abc---' ~~ /<ltm_ws1> | <ltm_ws2>/   # raku picks ltm_ws2 ('abc-'), mutsu ltm_ws1 ('abc---')
   ```
3. **`||` contributes only its first branch** (roast test 54):
   ```raku
   'food' ~~ / 'foo' | ['doof' || 'food'] /   # raku: 'foo'   mutsu: 'food'
   ```

Two adjacent engine gaps surfaced during this investigation and are folded in:

4. **No backtracking into shorter ends of the chosen branch** (the `Alternation` arm of
   `regex_match_atom_all_with_capture_in_pkg` collects ONE end per branch):
   ```raku
   "aaab" ~~ / [ a+ | q ] ab /   # raku: "aaab"   mutsu: no match
   ```
5. **Losing branches' plain `{ }` code blocks run** (candidate collection matches every
   branch fully, side effects included):
   ```raku
   my @ran; "abc" ~~ / 'ab' { @ran.push('b1') } x | 'abc' { @ran.push('b2') } /;
   # raku: @ran == ['b2']   mutsu: ['b1','b2']
   ```
   (#4 is fixed by this ADR's Slice 3. #5 is documented as a known gap with a sketched
   follow-up — see §6.)

## 2. Rakudo semantics, validated

Sources: black-box probe matrix run against rakudo v2026.06 (§7 acceptance matrix), and a
direct reading of `nqp/src/QRegex/NFA.nqp` (the NFA construction that *defines* LTM).
Key mechanics of the reference implementation:

- Each `|` branch gets a **declarative-prefix NFA**. At an alternation point the engine
  runs the combined NFA once and obtains, per branch (fate): the **longest prefix match
  length** and a **litlen** (see below). Branch attempt order = length desc, litlen desc,
  fate/declaration order asc. The winning branch is then matched *procedurally*; on
  failure (or later outer failure) the next-ranked branch is tried.
- **litlen** ("longest literal"): literal atoms emit `EDGE_CODEPOINT_LL` edges only while
  the build-time flag `$!LITEND == 0`; `regex_nfa` sets `LITEND := 1` for every node type
  except `literal`, `concat`, `alt` (NFA.nqp line ~174). So litlen counts the leading run
  of literal characters, where:
  - concatenation of adjacent literals extends it;
  - a nested `|` whose branches are all pure-literal extends it (per-branch, i.e. the
    litlen of whichever nested literal matched); any non-literal branch ends it at the
    recombination point (`method alt`, "stop litlen at recombination unless all alts are
    pure literal");
  - a **capture group `( … )` / `$<x>=[ … ]` ends litlen** (rxtype `subcapture` is not in
    the exempt set) **but is transparent for prefix *length*** (`method subcapture`
    descends);
  - **quantifiers end litlen** but participate fully in prefix length (loops/unrolling in
    `method quant`);
  - a **subrule call inlines the callee's own NFA including the callee's own `_LL`
    marks**, so literals leading the callee's body extend litlen (validated: `\w+ | <abb>`
    → the literal-bodied token wins the length tie).
- **Prefix construction rules** (what ends/extends the declarative prefix):

  | construct | prefix effect | validated by |
  |---|---|---|
  | literal, char class, `.`, ranges | consume, extend | probes 1–6 |
  | `( … )`, `$<x>=[ … ]`, `[ … ]` | transparent (descend); capture kills litlen | C, D, E, F |
  | quantifiers `* + ? ** n..m` (+ `%` separators) | loop/unroll, extend; kill litlen | G, 10, roast `<?> \| 'a' ** 1..2 'b'` |
  | `** {code}` | terminate | M |
  | nested `\|` | all branches participate | 9, K2 |
  | `X \|\| Y` | first branch **plus a zero-width ε-bypass** (NFA.nqp `method altseq`: builds child 0, then an ε from entry to exit) | 15, 16, roast 54 |
  | subrule call (token/rule/named) | inline callee NFA, cycle-guarded per name (`%seen` in `mergesubrule`); recursion cut = that call ends the path | 11, 28-probe, roast LTM-in-tokens tests |
  | `<.ws>` / `ws` (implicit sigspace or explicit, incl. method form) | **terminate** (NFA.nqp `subrule` special-cases `ws` → fate) | 18, roast 28 |
  | `<?before X>` (positive) | inline X, then terminate | 14 (with B) |
  | `<!before X>`, `<?after>`/`<!after>`, other negated zero-width | terminate (zero-width) | H (matches rakudo's own `#?rakudo todo` on roast line 461) |
  | plain `{ … }` block | terminate | A, ADR-0009 |
  | `<?{ … }>` / `<!{ … }>` | ε — transparent, keep measuring, never execute | B, ADR-0009 |
  | anchors `^ $ ^^ $$ << >>` | ε (transparent) | NFA.nqp `method anchor` |
  | backreference `$0` / `$<name>` | terminate | 22 |
  | non-constant `$var` interpolation | terminate (constants participate — inlined as literals at compile time) | 20/20c, roast 50 |
  | conjunction `&` | terminate (no NFA method → fate) | — |

- After ranking, **only the attempted branch runs procedurally** (side effects, captures);
  on its failure — including failure of what *follows* the alternation — the engine falls
  back to the next-ranked branch (roast: "sequential alternation first branch failure
  after LTM tries next best option"), and to *shorter ends within the same branch* first
  (probe I2: `"aaab" ~~ / [ a+ | q ] ab /` → `aaab`).

**Deliberate non-goals / accepted divergences from rakudo quirks:**

- No NFA construction. We measure prefixes with the existing backtracking matcher under
  `LTM_DECLARATIVE_MODE` (ADR-0009). Equivalent for ranking purposes: for declarative
  constructs, "max over all procedural match ends" = NFA longest match. A real token-NFA
  is a possible later perf project (§6), not correctness.
- litlen inside a **quantified subrule** counts in rakudo (merged `_LL` edges survive
  quantifier unrolling: `(\w+) | <tj> ** 2 x` picks the subrule branch) while a directly
  quantified literal does not (`(\w+) | 'ab' ** 2` picks the first branch). mutsu
  implements the simple rule — quantifiers always end the litlen chain, including around
  subrules. No roast test pins the quirk.
- `:m` (ignoremark) literals: rakudo has no `_M_LL` edge (XXX comments in NFA.nqp), so
  they never extend litlen. mutsu may treat them like `:i` literals (which do, via
  `_I_LL`); no test pins the difference.
- **mutsu-only quirk, found while implementing Slice 2**: a captureless, separator-less,
  single-atom fixed-count `atom ** N` is string-unrolled into literal repeated text by the
  pre-existing `expand_ltm_pattern` engine pass (`regex_parse_ltm.rs`, invoked from
  `regex_parse_core.rs` whenever `mode == RegexParseMode::Match`) *before* the token parser
  ever runs — this predates ADR-0022 and exists for matcher correctness/perf around small
  fixed bounds, unrelated to LTM ranking. When such a `**N` is the ENTIRE text of a
  standalone top-level pattern (`/'ab' ** 2/` parsed on its own), by the time
  `ltm_litlen_at` walks it the quantifier boundary is already gone — it is
  indistinguishable from a hand-written literal of the same expanded length, so litlen
  extends fully through it instead of stopping at 0. Verified NOT to affect the §7
  acceptance-matrix line it looks like it should break (`"abab" ~~ / (\w+) | 'ab' ** 2 /`):
  when `'ab' ** 2` is a *branch inside* a larger alternation rather than a standalone
  pattern, the anchored `expand_ltm_pattern` rewrite does not fire on it (its regex
  requires the whole current parse-unit text to be exactly `atom**count`), so branch
  ranking still measures litlen 0 there, correctly. The divergence is real but narrower
  than it first appeared: a `**N` literal quantifier alone as an entire pattern (rare) can
  rank differently than the same quantifier as one branch of an alternation (the common,
  ADR-relevant case). No roast test pins the standalone-pattern case; `+`/`*`/`?` are
  unaffected (`expand_ltm_pattern`'s trigger regex matches literal `**` only).

## 3. Current mutsu structure (what has to change)

Alternation is consumed via three code paths; all rank by longest end:

1. **Plural/backtracking path**: `regex_match_atom_all_with_capture_in_pkg`
   (`src/runtime/regex/regex_match_atom.rs:90`). Per branch, ONE end via the singular
   `regex_match_end_from_caps_in_pkg`, then
   `indexed.sort_by(|a,b| a.1.cmp(&b.1).then(b.0.cmp(&a.0)))` — end asc, index desc,
   returned lowest-priority-first (the engine's LIFO pops from the back). Consumers:
   `regex_match_core.rs` walk (`walk_tokens` candidate loops at ~505/556/1028),
   `regex_match_sep.rs`, `regex_match_capture.rs:135/574`.
2. **Singular path**: the `Alternation` arm of `regex_match_atom_with_capture_in_pkg`
   (`src/runtime/regex/regex_match_capture.rs:100`) — "longest end wins, first-declared
   on tie". Consumer: quantifier iteration growth (`grow_one_iter`,
   `regex_match_core.rs:831`), i.e. `[a|b]+` iterations.
3. **No-capture probe**: the `Alternation` arm of `regex_match_atom_in_pkg`
   (`src/runtime/regex/regex_match_atom_simple.rs:193`) — longest end.

Existing machinery to build on (ADR-0009):

- `LTM_DECLARATIVE_MODE` / `LTM_PREFIX_TERMINATED` thread-locals
  (`regex_helpers.rs:24/28`), honored today only by the `CodeAssertion` arm
  (`regex_match_capture.rs:253`: assertion → zero-width pass; plain block → set
  TERMINATED) and by `walk_tokens`'s unwind check (`regex_match_core.rs:457`).
- `declarative_prefix_match_len(&mut self, pattern: &str, text: &str) -> (Option<usize>, bool)`
  (`regex_resolve.rs:139`) — string-based, start-anchored; used by protoregex dispatch.
- Subrule parse/candidate memoization (`parsed_subrule_candidates`, PARSED_TOKEN_CANDIDATES).

Important context for the implementer: the sort-visible tie today is *accidentally
passing* roast's "LTM - literal wins tie against `\w*`" (test 21) in-file while the same
construct fails as a one-liner — do not trust the current green; after this change it
must pass for the right reason (litlen through subrule descent).

## 4. Decision

Rank `|` branches at match time by the triple

```
(prefix_len desc, litlen desc, declaration index asc)
```

where both `prefix_len` and `litlen` are measured against the actual input at the current
position, using the existing matcher under an *extended* `LTM_DECLARATIVE_MODE` — no NFA.
Then produce backtracking candidates per branch with the **plural** ends enumeration,
ordered branch-major by rank.

### 4.1 New measurement API (Slice 1)

`src/runtime/regex/regex_ltm_rank.rs` (new file), `impl Interpreter`:

```rust
/// Longest declarative-prefix match of `pattern` at `pos`, plus whether the
/// measurement was cut short by a non-declarative atom (=> cannot be used to
/// filter the branch out). Runs the ordinary ends-enumeration matcher under
/// LTM_DECLARATIVE_MODE; never executes user code (ADR-0009 discipline).
pub(crate) fn ltm_prefix_len_at(
    &mut self,
    pattern: &RegexPattern,
    chars: &[char],
    pos: usize,
    pkg: &str,
) -> (Option<usize>, bool)
```

Implementation: save/replace the two thread-local flags exactly as
`declarative_prefix_match_len` does (they must nest — a measurement can occur inside a
real match inside another measurement), call
`regex_match_ends_from_caps_in_pkg(pattern, chars, pos, pkg)`, take the **max** end,
subtract `pos`. Keep the string-based `declarative_prefix_match_len` for the proto path
(or reroute it through this later — not required).

### 4.2 Extend LTM_DECLARATIVE_MODE semantics (Slice 1)

Today only code atoms are neutralized; the prefix table in §2 needs the other stoppers.
Add mode-guarded early behavior for these atoms — in **both** capture-bearing atom
matchers (`regex_match_atom_all_with_capture_in_pkg` and
`regex_match_atom_with_capture_in_pkg`) and the no-capture prober
(`regex_match_atom_in_pkg`), via one shared helper so the three stay in sync:

```rust
/// In LTM declarative mode, how this atom participates in prefix measurement.
enum LtmAtomMode { Normal, Terminate, TerminateAfter(&RegexPattern) /* positive lookahead */ }
fn ltm_atom_mode(atom: &RegexAtom) -> LtmAtomMode
```

- `Terminate` (zero-width success + set `LTM_PREFIX_TERMINATED`, so `walk_tokens`
  unwinds and the length measured so far stands): `WsRule`; `Named` naming `ws` in
  any lookup form — reuse the normalization in `named_lookup_is_ws`
  (`regex_parse_core.rs:401`, strips `.`/`&` prefixes; verified: sigspace inserts
  `WsRule`, explicit `<.ws>`/`<ws>` arrive as `Named`);
  `Backref` / `NamedBackref`; `VarInterp`; `ClosureInterpolation`; `Conjunction`;
  `RegexQuant::RepeatCode` (token-level check, since the quant sits on the token, not
  the atom); negated or behind `Lookaround`; `CodeAssertion { is_assertion: false }`
  (already done).
- `TerminateAfter(inner)`: positive ahead `Lookaround` — measure `inner`'s ends from
  `pos` as consuming, then terminate.
- `CodeAssertion { is_assertion: true }`: zero-width pass without executing (already done).
- `SequentialAlternation` in mode: candidates = ends(first branch) ∪ {pos} (the ε-bypass),
  and it does NOT set TERMINATED by itself (the ε keeps the measurement alive past the
  group). This cannot cause a false *filter*: with the ε the group can never be the sole
  reason a fully-declarative measurement returns None.

Anchors, groups, capture groups, quantifiers, char classes, nested `|`: already correct
via normal matching (transparent / consuming). Note `^`/`$` atoms match normally in
measurement — measurement starts at the real current `pos` on the real subject, so
anchors evaluate exactly as in the real match.

The `LTM_PREFIX_TERMINATED` flag today means "stopped at a *code* atom" to its one
caller; after this slice it means "stopped at any non-declarative atom". Keep the
ADR-0009 contract: **a terminated measurement can order but never filter**
(`(None, true)` ⇒ keep the branch, ranked at prefix length measured so far / 0).

### 4.3 litlen chain (Slice 2)

Same new file:

```rust
/// Length of the leading-literal match of `pattern` at `pos`: how many input
/// chars are consumed by the longest path through the pattern's leading
/// literal region (per §2: concat of literals; nested alternation branches that
/// are themselves pure leading-literal, taking the matched branch's length;
/// non-capturing groups descend; subrule calls descend into the callee's own
/// leading literal region, cycle-guarded; capture groups, quantifiers, char
/// classes, and every other construct end the region).
fn ltm_litlen_at(&mut self, pattern: &RegexPattern, chars: &[char], pos: usize,
                 pkg: &str, seen: &mut HashSet<String>, depth: usize) -> usize
```

This is a direct char-comparison walk (respect `ignore_case` on comparison; `ignore_mark`
literals may either count or not — see §2 non-goals), NOT a matcher run. Token loop from
the front: `RegexAtom::Literal(c)` with `RegexQuant::One`, no separator, no
capture aliases on the token → compare with `chars[pos + acc]`, mismatch ⇒ **return 0
for the whole chain? No** — mismatch means this literal path does not match here; return
`acc`… — careful: rakudo's litlen is the literal length *along the run that achieved the
prefix match*. If a leading literal fails to match the input, the branch's prefix match
(4.1) either failed too (fully-literal head) or matched via another nested-alt path. Rule:
on mismatch inside a nested alternation, that nested branch contributes nothing and the
max over the other nested branches is used; on mismatch in the top-level chain, the chain
value is what matched so far only if the overall prefix measurement also stopped there —
in practice return `acc` at first mismatch; the ranking only ever consults litlen between
branches whose `prefix_len` already tied, which bounds any imprecision to genuinely
ambiguous quirk territory.
`Group(p)` → descend (continue chain only if the group consumed its entire own chain —
i.e. the group's contribution ended exactly at a group boundary with no non-literal left
before its end; simplest faithful rule: descend into `p` recursively; if `p`'s chain walk
reached `p`'s end without hitting a region-ender, continue the outer chain, else stop
after adding the inner contribution).
`Alternation(alts)` → recursively evaluate each branch's chain; if every branch is
pure-literal-to-its-end, contribution = the longest branch value that actually matched
the input here, and the chain continues; otherwise contribution = best matching branch
value and the chain stops (mirrors `method alt`'s litendback rule).
`Named(name)` → resolve via the existing memoized subrule machinery
(`parsed_subrule_candidates`); descend into the callee pattern with `seen` guarding
recursion (on cycle: stop chain) and `depth` capped (e.g. 16). Multiple candidates
(proto): max over candidates.
Everything else → stop.

### 4.4 Apply ranking in the three consumers (Slice 3)

**(a) `regex_match_atom_all_with_capture_in_pkg` Alternation arm** — the main change:

```text
for (i, alt) in alternatives:                      # keep pure-code-block deferral as-is
    (plen, stopped) = ltm_prefix_len_at(alt, chars, pos, pkg)
    if plen is None and !stopped: skip branch      # fully-declarative non-match (sound filter)
    rank_key = (plen.unwrap_or(0), ltm_litlen_at(...), i)
    ends = regex_match_ends_from_caps_in_pkg(alt, chars, pos, pkg)   # PLURAL (fixes gap #4)
    …collect (rank_key, ends)
sort branches by (plen desc, litlen desc, i asc)
emit LIFO lowest-priority-first: for branch in reverse-rank order:
    for (end, caps) in branch.ends (already highest-priority-first) → reversed
    → push, so the top of the stack is the best branch's most-preferred end,
      then its shorter ends, then the next branch — matching rakudo's
      "backtrack within the chosen branch before falling to the next fate".
```

Fully-declarative fast path (perf): if a cheap recursive scan finds no stopper anywhere
in the branch (`branch_is_fully_declarative(alt)` — a pure tree walk, no matching), then
`prefix_len = max end of the full ends enumeration` — reuse `ends`, zero extra matcher
work. Only stopper-bearing branches pay a second (prefix) run. The scan itself is
O(pattern size) per call; if profiling shows it hot, memoize per `Arc<RegexPattern>`
identity later — do not add a raw-pointer cache (ParseMemo lesson, PR #6132).

Dedup: today's arm dedups implicitly by sort; keep candidates with equal `end` from
*different* branches distinct only if their capture deltas differ — simplest correct
behavior: keep the current no-dedup (all candidates flow; the engine already tolerates
duplicates elsewhere, cf. the CaptureGroup arm's dedup comment) but preserve order.

**(b) singular arm** (`regex_match_capture.rs:100`): compute the same per-branch rank,
pick the best-ranked branch that matches, return its greedy end (replace the
`next > best_next` longest-end rule).

**(c) no-capture arm** (`regex_match_atom_simple.rs:193`): same selection; return the
chosen branch's greedy end. (This changes observable *ends* for shapes like problem #2
even in probe contexts — intended.)

`SequentialAlternation` runtime arms stay untouched (already declaration-ordered).

### 4.5 Re-entrancy and threads

The measurement flags are thread-locals with save/restore; 4.1 must follow the same
discipline. The measurement itself calls back into the full matcher (subrules, closures
are NOT executed in mode — verify `ClosureInterpolation` terminates rather than
evaluates). No new global state; no cross-thread anything.

## 5. Implementation slices (each a PR; order matters)

1. **Slice 1 — mode extension + `ltm_prefix_len_at`** (`regex_ltm_rank.rs`,
   `ltm_atom_mode` guards in the three atom matchers). Pin: extend
   `t/regex-ltm-declarative-prefix.t` with ws/backref/lookahead/`||`-ε cases measured via
   a proto-dispatch shape (the only current consumer), or unit-test the new API with
   `#[test]` in the new module. No behavior change to `|` yet. Must stay green on
   `S05-grammar/protoregex.t` (whitelisted).
2. **Slice 2 — `ltm_litlen_at`** with `#[test]` unit coverage (literal chain, nested alt,
   subrule descent, cycle guard, `:i`).
3. **Slice 3 — ranking swap in the three arms + plural ends** (the semantic change).
   Pin: new `t/regex-ltm-alternation.t` from the §7 matrix (raku-verified expectations).
   Acceptance: `roast/S05-metasyntax/longest-alternative.t` 28 and 54 flip to ok
   (50 needs Slice 5; line-461 negative-lookahead stays `#?rakudo todo` — mutsu matches
   rakudo's current behavior); `Cro::HTTP` `t/http-router.rakutest` → 83/83
   (run via `bash tmp/cro-suite-run.sh http`, single-instance rule). Watch for flips in:
   whitelisted `S05-*` files, grammar-heavy roast (`S05-grammar/*`, `integration/*`),
   and the JSON/YAML batteries suites (`scripts/battery-testsuite.sh`) — CI covers all;
   fix forward.
4. **Slice 4 — BLOCKERS.md + ledger update**: refresh the `longest-alternative.t` row
   (currently stale), note remaining 50/461.
5. **Slice 5 (optional, separate) — non-constant interpolation marking**: thread
   interpolated-span info out of `interpolate_bound_regex_scalars`
   (`regex_interpolate.rs:288` — today it returns only the substituted pattern String)
   so the regex parser can flag tokens born from a runtime variable as non-declarative
   (a `from_runtime_interpolation` bool on `RegexToken`), which `ltm_atom_mode` then
   treats as Terminate. `constant`-declared values keep participating. Fixes roast
   test 50; enables whitelisting `longest-alternative.t` (with 461 fudged upstream).
   If span-threading proves too invasive, an acceptable interim is a per-pattern side
   list of interpolated char ranges consulted at parse time — decide at implementation.

Perf: iterate with the **debug** binary + `MUTSU_VM_STATS` where useful; wall-clock only
on release; document nothing from local runs — after merge, read the bench CI
(`git show origin/bench-data:bench-history.tsv`), expecting the regex-heavy rows
(yaml-parse, grammar benches) to be the sensitive ones. The fully-declarative fast path
in 4.4(a) is the guard: a branch with no stoppers pays only the (new) tree scan.

## 6. Known gaps this ADR does NOT close

- **Side effects of losing branches** (problem #5): fixing it means not fully matching
  branches that lose the ranking until the engine actually attempts them — i.e. lazy
  candidate production in the walk loop, an engine-shape change. Sketch: have the
  Alternation arm return rank-ordered *branch descriptors* and let `walk_tokens`
  materialize a branch's ends on first pop. Do it as its own ADR/slice if a real test
  demands it; note that Slice 3 does not make this worse (it already runs today).
- **A real token-NFA** for O(1)-ish ranking over many-branch alternations (the Cro
  router compiles ~80 routes into one alternation, ranked per request): perf follow-up,
  only if bench CI shows the measurement pass hot.
- Rakudo quirk parity listed in §2 non-goals (quantified-subrule litlen, `:m` litlen).

## 7. Acceptance matrix (raku v2026.06-verified; becomes `t/regex-ltm-alternation.t`)

Each line: expression → expected winner/result (raku-verified 2026-08-09).

```raku
# ties broken by litlen
"ab" ~~ / (\w\w) | 'ab' /                       # literal branch ($0 undefined)
"ab" ~~ / 'ab' | (\w\w) /                       # literal branch
"/category/tree" ~~ / "/category/" (\w+) | "/category/tree" /   # literal branch
"abc" ~~ / 'a' (\w\w) | 'abc' /                 # literal branch (capture kills litlen)
"abc" ~~ / 'a' \w\w | ('abc') /                 # FIRST branch (capture kills b2's litlen)
"aab" ~~ / 'a' \w \w (<?>) | 'aa' \w /          # second branch (litlen 2 > 1)
# capture groups transparent for length
"/xy" ~~ / "/" \w | ("/xy") /                   # capture branch, matched "/xy"
# groups/nested alternation extend litlen
"/category/tree" ~~ / "/category/" (\w+) | "/category/" [ 'tree' ] /   # group branch
"/c/tree" ~~ / "/c/" (\w+) | "/c/" [ 'tree' | 'x' ] /                  # nested-alt branch
# quantifiers: length yes, litlen no
"abab" ~~ / (\w+) | 'ab' ** 2 /                 # first branch (litlen tie 0-0, order)
"aab" ~~ / 'a' | 'a' ** 1..2 'b' /              # second branch, matched "aab" (length 3)
# subrule descent (length + litlen)
my token abb { 'abb' }; "abb" ~~ / (\w+) | <abb> /       # <abb> wins ($<abb> defined)
# code atoms
"abcd" ~~ / 'ab' { ; } \w\w | 'abc' /           # 'abc' (block terminates prefix)
"abcd" ~~ / 'ab' <?{ True }> \w\w | 'abc' /     # first branch, matched "abcd" (ε)
# lookahead
"abcd" ~~ / 'ab' <?before c> (\w\w) | 'abc' /   # 'abc' (litlen 3 beats 2 at length tie 3)
"abcde" ~~ / ab <![e]> cde | ab.. /             # "abcd" (negated lookahead terminates)
# sequential alternation inside |
"food" ~~ / 'foo' | ['doof' || 'food'] /        # 'foo'
"food" ~~ / 'foo' | ['food' || 'doof'] /        # 'food'
# ws stopper
my rule r {\w+ '-'+}; my token t {\w+ '-'}; "abc---" ~~ /<r>|<t>/   # <t>, "abc-"
# fall to next best when the winner's tail fails
"food" ~~ / (f\w+) x | 'foo' /                  # 'foo'
# within-branch backtracking preserved across the alternation
"aaab" ~~ / [ a+ | q ] ab /                     # "aaab"
# ranking never overrides leftmost-position scan
"xab" ~~ / 'ab' | b /                           # "ab" (position 1 beats position 2)
# :i
"AB" ~~ m:i/ (\w\w) | 'ab' /                    # literal branch
# ratchet interaction unchanged (roast S05 486-489 already pass)
'ab' ~~ / [ab | a ]: b /                        # Nil
```

## 8. Alternatives rejected

- **Static branch reordering at parse time** (sort branches by static literal prefix):
  cannot express input-dependent prefix lengths (`\w+` vs literal depends on the
  subject); wrong on every "longer quantified atom wins" case.
- **Keep longest-end ranking + litlen tie-break only**: fixes the Cro case but not
  problem #2/#3 (prefix ≠ full length); roast 28/54 stay red.
- **Full NFA now**: highest fidelity + best perf, but a large new engine component;
  the matcher-under-mode measurement reuses ~everything and is behavior-equivalent for
  ranking. NFA remains available as a follow-up optimization (§6).
