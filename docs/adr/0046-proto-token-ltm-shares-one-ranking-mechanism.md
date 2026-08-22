# ADR-0046: Proto-token dispatch shares the one LTM ranking mechanism, and interpolation provenance covers arrays and token bodies

- **Status**: **Implemented (all five slices landed; Slice 4-5 on 2026-08-22).**
  Every row of §2.1, §2.2 and §2.3 now answers as `raku` does, on both proto
  dispatch mechanisms, and the headline repro in §5 flips. The per-slice history
  below is kept for the implementation notes each one accumulated.
  **Slice 1 (array/regex-object
  interpolation provenance, §4 item 1) landed 2026-08-20**: the three
  `push_regex_interpolated_alternation` call sites (`@name`, `@$var`,
  `@(...)`) and `array_var_alternation_atom` (`<@var>` / `<?@var>` /
  `<!@var>`) now set `RegexToken::from_runtime_interpolation`, and the
  `<$var>` regex-value reroute was found to need (and got) the same
  unconditional marking (§2.1 probe S). Pin: `t/regex-ltm-interpolation-provenance.t`
  (all 8 probes I/J/K/L/M/Q/R/S, dual-oracle verified against `raku`). A
  latent bug this fix unmasked (fixing `array`'s own quantifier-separator
  interpolation, and a pre-existing, unrelated LTM over-crediting gap) was
  fixed/documented in the same slice — the latter was deferred to Slices 3/4 and
  is written up in
  `news/2026-08/subtracted-char-class-terminates-ltm-prefix.md`.
  **Slices 2 and 3 landed 2026-08-22**: `interpolate_bound_regex_scalars` now
  marks its substitutions (`scalar` row of §2.2 green on mechanism 1), and
  mechanism 1 (`eval_token_call_values_at`) ranks via the shared
  `ltm_branch_rank_key` primitive, gaining the litlen tie-break (`litlen` row
  green). Slice 3 also had to make `RegexAtom::VarDecl` a new
  `LtmAtomMode::SkipZeroWidth` — zero-width, non-terminating, and *not
  executed* — because the parsed-pattern measurement path no longer goes
  through `regex_match_with_captures`'s string-level declarator strip; that
  closed a real ADR-0009 leak where measuring `token TOP { :my $x = …; <inner> }`
  wrote `$x` into the live env and changed what `inner` interpolated (stale
  `todo` in `t/regex-inline-code-block.t` removed). Pins:
  `t/regex-ltm-proto-dispatch.t`, `t/regex-ltm-interpolation-provenance.t`
  probes T/U.
  **Slices 4 and 5 landed 2026-08-22**: the nested-`<name>` proto loop is
  rank-then-match, so mechanism 3 measures a declarative prefix at last and a
  losing candidate no longer runs; three corrections it forced (`<sym>` as a
  named capture, subtracted char classes terminating the prefix, and the
  `||`-epsilon `(None, false)` filter unsoundness) are in §4 item 4. Both
  originating `todo/deep` files were retired to `news/2026-08/`.
- **Context**: the originating finding is now
  `news/2026-08/proto-token-ltm-one-ranking-mechanism.md` (it started life as
  `ltm-inline-unbounded-quantifier-vs-array-tie.md`, whose recorded root cause this ADR
  corrects). Builds directly on [ADR-0009](0009-regex-code-assertion-execution-model.md)
  (`LTM_DECLARATIVE_MODE`, "measuring a candidate must never execute it") and
  [ADR-0022](0022-regex-alternation-ltm-ranking.md) (the `(prefix_len, litlen,
  declaration order)` ranking triple and its measurement primitives), both shipped.
- **Scope**: how *proto token / proto regex* candidates are ranked, and how a regex
  records that a span of its pattern came from interpolating a runtime variable.
  ADR-0022's `|`-alternation ranking is not re-litigated — it is the mechanism the other
  two consumers are being moved onto.

## 1. Problem

ADR-0022 fixed `|` alternation and explicitly left proto dispatch alone ("Protoregex
dispatch already ranks by declarative prefix (ADR-0009) and is NOT changed here;
interpolated-`@array` alternation already approximates LTM by length-sorting and is not
changed here"). Both halves of that carve-out turn out to be wrong, and mutsu today has
**three** LTM ranking implementations with three different algorithms:

| # | site | algorithm | declarative mode? | litlen? |
|---|---|---|---|---|
| 1 | `dispatch.rs::eval_token_call_values_at` (`:rule<...>` / outermost proto entry) | declarative **prefix length** only, decl-order tie-break | yes (`declarative_prefix_match_len`, string-based) | **no** |
| 2 | `regex_match_atom.rs::ltm_rank_and_collect_branches` (`\|` alternation, ADR-0022) | `(prefix_len, litlen, decl order)` | yes (`ltm_prefix_len_at`, pattern-object based) | yes |
| 3 | `regex_match_atom.rs` nested-`<name>` proto candidate loop (`regex_match_atom.rs:541-599`) | **longest actual match end**, decl-order tie-break | **no** | **no** |

Mechanism 3 is the one every nested subrule call goes through — i.e. essentially every
real grammar. It never sets `LTM_DECLARATIVE_MODE` at all: it runs
`regex_match_ends_from_caps_in_pkg` on *every* candidate for real and sorts by end.
That is the pre-ADR-0009 algorithm, still live, and it is invisible to
`roast/S05-grammar/protoregex.t` because every LTM assertion in that file uses
`subparse(:rule(...))`, which enters through mechanism 1.

Separately, the "provenance" mechanism ADR-0022 Slice 5 introduced — a
`NON_DECLARATIVE_INTERP_MARK` sentinel that makes the tokenizer set
`RegexToken::from_runtime_interpolation`, which `walk_tokens` treats as a prefix stopper —
was wired into exactly one of mutsu's interpolation sites. Array interpolation and
grammar/token-body interpolation both still produce structure indistinguishable from
hand-written literals.

### 1.1 Correcting the recorded root cause

The predecessor todo file attributed the divergence to Rakudo's NFA giving an *inline
unbounded quantifier* (`<-[;]>+`) a structural priority over a *bounded* candidate on a
runtime length tie, and concluded that matching it would need "genuine static analysis of
each candidate's quantifier structure". **That theory is wrong and should not be
implemented.** There is no length tie: in Rakudo the `@opts` interpolation *terminates*
the candidate's declarative prefix at 4 characters (`'Foo='`), so `<-[;]>+`'s prefix of 10
wins outright on plain ADR-0022 rules. The todo file's "contrast case" (a named-subrule
catch-all supposedly flipping Rakudo's preference to `known`) does not reproduce either —
raku answers `OTHER` for that shape too (probe C below).

## 2. Rakudo semantics, validated

All probes run against the system `raku` and `target/debug/mutsu` at
`4e4ca30ce` (repro scripts under `tmp/`, not committed).

### 2.1 Array interpolation always terminates the declarative prefix

Subject `"StrictX"`; branch 2 is a fixed `'St'` (prefix 2). If the leading array
interpolation participated, branch 1's prefix would be 7 and it would win.

| # | pattern | raku | mutsu | verdict |
|---|---|---|---|---|
| I | `/ @opts 'X' \| 'St' /` (`my @opts`) | `St` | `StrictX` | terminates in raku |
| J | `/ @copts 'X' \| 'St' /` (`constant @copts`) | `St` | `StrictX` | terminates **even for a `constant`** |
| K | `/ <@opts> 'X' \| 'St' /` | `St` | `StrictX` | the assertion form terminates too |
| M | `/ @ropts 'X' \| 'St' /` (array of `Regex`) | `St` | `StrictX` | element type is irrelevant |
| L | `/ [ 'Strict' \| 'Lax' \| 'None' ] 'X' \| 'St' /` (hand-written) | `StrictX` | `StrictX` | literal alternation *does* participate |
| Q | `/ @(<Strict Lax>) 'X' \| 'St' /` | `St` | `StrictX` | contextualizer terminates |
| R | `/ @$ref 'X' \| 'St' /` | `St` | `StrictX` | deref form terminates |
| S | `/ <$rx> 'X' \| 'St' /` (`$rx = rx/Strict/`) | `St` | `StrictX` | `<$var>` regex-value form terminates |

The rule is simpler than the scalar rule ADR-0022 §2 recorded: **every array/regex-object
interpolation form terminates, unconditionally.** No constant-vs-non-constant analysis is
needed for `@` — a `constant @a` terminates exactly like a `my @a` (probe J), because
Rakudo compiles the interpolation to an `INTERPOLATE` subrule call that has no NFA method,
not to inlined literal alternatives. Contrast the `$`-scalar case, where a genuine
`constant` *is* inlined and does participate (ADR-0022 Slice 5, unchanged here).

Probe L is the important negative control: mutsu is not merely "too eager about
interpolation", it is producing **exactly the structure of probe L** — the interpolator
splices the array's elements into the pattern text as a literal alternation
(`push_regex_interpolated_alternation`, `regex_parse_ltm.rs:62`), so by the time anything
measures the prefix the interpolation boundary no longer exists.

### 2.2 Proto candidates are ranked by declarative prefix, then litlen — at every call site

Grammar shape: `proto token val {*}` with two candidates and an actions class that
`make`s the candidate's name, dispatched two ways — `parse(:rule<val>)` (mechanism 1) and
a nested `<val>` inside `token TOP` (mechanism 3).

| # | candidates | expected (raku) | mutsu mech 1 | mutsu mech 3 |
|---|---|---|---|---|
| code | `'Foo=' {} 'Strict'` vs `<-[;]>+` | `OTHER` | `OTHER` ✓ | **`KNOWN`** ✗ |
| ws | `'Foo=' <.ws> 'Strict'` vs `<-[;]>+` | `OTHER` | `OTHER` ✓ | **`KNOWN`** ✗ |
| scalar | `'Foo=' $opt` vs `<-[;]>+` | `OTHER` | **`KNOWN`** ✗ | **`KNOWN`** ✗ |
| array | `'Foo=' @opts` vs `<-[;]>+` | `OTHER` | **`KNOWN`** ✗ | **`KNOWN`** ✗ |
| litlen | `\w\w\w` vs `'abc'` on `"abc"` | `LIT` | **`CC`** ✗ | **`CC`** ✗ |

The `code`/`ws` rows isolate mechanism 3's total absence of declarative-mode measurement:
those two stoppers are the *oldest* ones mutsu implements (ADR-0009), mechanism 1 honours
them, and mechanism 3 still ignores them. The `litlen` row isolates mechanism 1's missing
second tie-break.

### 2.3 Losing proto candidates must not run

```raku
my @ran;
grammar H {
    proto token v {*}
    token v:sym<x> { 'ab' { @ran.push('x') } 'c' }
    token v:sym<y> { 'abc' { @ran.push('y') } }
    token TOP { <v> }
}
H.parse('abc');
# raku: @ran == ["y"]    mutsu: ["x", "y"]
```

Mechanism 3 matches every candidate to completion before choosing, so every losing
candidate's plain `{ }` blocks fire — and, under `:actions`, so do the action methods of
the subrules it reduced along the way. This is the same class as ADR-0022 §6's deferred
gap #5, but it is *worse* here (proto candidates are whole named rules with actions,
not inline branch fragments) and, unlike gap #5, it is fixed for free by Decision 1
below: once ranking is a measurement, only the winner is executed.

## 3. Decision

### Decision 1 — one ranking mechanism, three call sites

`ltm_branch_rank_key(&mut self, alt: &RegexPattern, chars, pos, pkg) -> (usize, usize)`
(`regex_ltm_rank.rs:324`, ADR-0022) becomes the *sole* LTM ranking primitive. Every
candidate set — `|` branches, `:rule<...>` proto entry, nested `<name>` proto dispatch —
is ordered by `(prefix_len desc, litlen desc, declaration index asc)`, the last coming
free from a stable sort over candidates in declaration order.

**Mechanism 3 (`regex_match_atom.rs:541-599`) is restructured from "match everything, sort
by end" to "rank by measurement, then match the winner".** Concretely, inside the existing
left-recursion loop, for the `has_proto` case:

1. For each candidate `(parsed, sub_pkg, sym_key)`, compute
   `ltm_branch_rank_key(parsed, chars, pos, sub_pkg)`. This runs under
   `LTM_DECLARATIVE_MODE` and therefore executes nothing (ADR-0009).
2. Drop a candidate whose measurement returned `(None, false)` — a sound "cannot match
   here" verdict. Keep `(None, true)` ranked at 0 (ADR-0022 §4.1's contract: a terminated
   measurement can order but never filter).
3. Stable-sort by rank descending, then attempt candidates in that order with the real
   matcher, stopping at the first that yields a non-empty end set. On failure of what
   *follows* the subrule call, the engine falls back to the next-ranked candidate — so the
   arm must return the ranked candidates lazily-ordered rather than only the winner's ends
   (keep today's "return a candidate vector, lowest-priority-first" contract; materialize
   the lower-ranked candidates' ends only when the engine pops down to them).

Step 3's laziness is what actually buys §2.3: it is the same "materialize a branch's ends
on first pop" shape ADR-0022 §6 sketched for `|`, but it is cheaper to do here because a
proto candidate is already an independently-parsed `RegexPattern` with a `sym_key`, so
there is nothing to thread through `walk_tokens`.

**Mechanism 1 (`dispatch.rs::eval_token_call_values_at`)** keeps its role (it is the entry
point that must also return the winning candidate's `:sym<...>` for action dispatch) but
swaps `declarative_prefix_match_len(&pattern, text)` for parse-then-`ltm_branch_rank_key`,
gaining the litlen tie-break. The candidate patterns are already parsed and memoized
downstream (`parsed_subrule_candidates` / `PARSED_TOKEN_CANDIDATES`), so this removes a
duplicated parse rather than adding one. `declarative_prefix_match_len` then has no
callers outside `regex_match_public.rs:316` and should be reduced to a thin wrapper over
`ltm_prefix_len_at` or retired.

**Rejected alternative — teach each mechanism its own tie-breaks.** That is what produced
the current three-way divergence: mechanism 1 grew ADR-0009's stoppers, mechanism 2 grew
ADR-0022's full triple, mechanism 3 grew neither, and no test covered the difference
because roast exercises proto LTM only through `:rule(...)`. Any future LTM refinement
would have to be applied three times and would silently be applied twice.

**Rejected alternative — build a real token NFA now.** Still the right long-term answer
for *performance* (ADR-0022 §6), still not needed for correctness: the
matcher-under-declarative-mode measurement is behaviour-equivalent for ranking and is
already shipped and test-covered. Note this ADR *improves* mechanism 3's cost profile
rather than worsening it: today it runs N real matches per proto call site; afterwards it
runs N measurements (which terminate early at the first stopper) plus one real match.

### Decision 2 — interpolation provenance belongs to every interpolation site

`RegexToken::from_runtime_interpolation` is the right primitive (it is checked in
`walk_tokens`, `regex_match_core.rs:483`, which every top-level and nested token walk
funnels through, so one flag covers arbitrary nesting). It is under-applied, not
mis-designed. Three sites must set it:

1. **Array/contextualizer splices in `interpolate_regex_scalars`** — the three
   `push_regex_interpolated_alternation` call sites (`regex_parse_modifier.rs:612`
   for `@$var`, `:668` for `@name`, `:704` for `@(...)`). Wrap the spliced span in
   `NON_DECLARATIVE_INTERP_MARK` exactly as the `$`-scalar arms at `:405/:426` and
   `:505/:526` already do. **Unconditionally** — per §2.1 probe J there is no
   `is_compile_time_constant_scalar` analogue for `@`.
2. **`interpolate_bound_regex_scalars`** (`regex_interpolate.rs:288`), the substitution
   used for grammar/token bodies via `eval_token_def` (`dispatch.rs:256/262/276/282`) and
   `regex_token_resolve.rs:420`. ADR-0022 Slice 5 deliberately targeted only the
   general-case interpolator; this is the gap that leaves the `scalar` row of §2.2 red on
   *both* proto mechanisms. Same sentinel, same `constant` exemption as the general case.
3. **`array_var_alternation_atom`** (`regex_parse_core.rs:351`, the `<@var>` / `<?@var>` /
   `<!@var>` forms). There is no text splice here — it builds a `RegexAtom::Alternation`
   (or a collapsed char class) directly — so it must set `from_runtime_interpolation` on
   the `RegexToken` it is placed in rather than emit a sentinel. Because the flag lives on
   the token and the atom is built by a helper that does not own the token, the cleanest
   shape is for the helper to return `(RegexAtom, bool)` (or a small
   `InterpolatedAtom { atom, from_runtime_interpolation }`) and for both call sites
   (`regex_parse_core.rs:2508`, `:2865`) to propagate it. The same treatment applies to
   the `<$var>` regex-value reroute (probe S).

**Rejected alternative — a dedicated `RegexAtom::RuntimeInterpolation` wrapper variant.**
It would be more self-describing than a bool on the token, but it needs a matching arm in
every atom matcher (three of them) plus `ltm_atom_mode`, `ltm_litlen_walk`, and the
`try_collapse_alternation_to_charclass` path, for no behavioural gain over a flag that one
chokepoint already reads. Revisit only if provenance ever needs to carry *more* than one
bit (e.g. "which variable", for an error message).

**Known residual, accepted:** ADR-0022 Slice 5's documented `"$var..."` limitation (a
sentinel placed inside a double-quoted regex literal is swallowed by the tokenizer's own
quote-scanning inner loop, so it stays declarative — `// TODO:` at
`regex_parse_modifier.rs:398`) is unchanged by this ADR and applies to the `@` arms too.
It is a separate, narrower fix in the tokenizer's `"..."` arm.

### Decision 3 — provenance before unification

Slice order matters. Doing Decision 1 first would move mechanism 3 onto a measurement that
*still* cannot see array or token-body interpolation, so the headline repro would stay red
and the change would look ineffective; doing Decision 2 first turns the `scalar`/`array`
rows of §2.2 green on mechanism 1 immediately and gives Decision 1 a correct measurement
to unify onto.

## 4. Implementation slices

1. **Slice 1 — array interpolation provenance (landed 2026-08-20)** (Decision 2 items 1
   and 3). Pin: `t/regex-ltm-interpolation-provenance.t` carrying §2.1's probes
   I/J/K/L/M/Q/R/S. Expected flips: the `array` row of §2.2 mechanism 1 turns green;
   mechanism 3 stays red. Watch: `roast/S05-metasyntax/longest-alternative.t`
   (whitelisted, 62/62 — still green) and the `<@var>`-using batteries (YAMLish's
   grammar, `t/yaml-battery.t` — still green) — an array interpolation that now
   terminates the prefix can reorder a branch choice that previously happened to be
   right.
   - **Implementation notes beyond the plan:** (a) wrapping the spliced alternation in
     `NON_DECLARATIVE_INTERP_MARK` *before* calling `push_regex_interpolated_alternation`
     hides the preceding `||`/`|` from that function's own separator sniffing — the mark
     must be inserted after the separator lookup runs (see
     `push_regex_interpolated_alternation_marked` in `regex_parse_ltm.rs`), or
     `roast/S05-metasyntax/sequential-alternation.t` and `litvar.t` regress. (b) the
     interpolation-span quantifier-wrap arm (the `NON_DECLARATIVE_INTERP_MARK` closing
     handler in `regex_parse_core.rs`) never consumed a trailing `%`/`%%` separator on a
     quantified interpolated array (`@arr ** 4 % sep`); factored the separator-consuming
     logic out of the main per-atom loop into `consume_repeat_separator` so both sites
     share it. (c) also found and documented (not fixed — out of Slice 1's scope, folded
     into future Slice 3/4 work) a pre-existing, unrelated LTM gap where a bare
     named-subrule call with an unbounded quantifier and no internal stopper wrongly gets
     full/greedy ranking credit — see
     `news/2026-08/subtracted-char-class-terminates-ltm-prefix.md`;
     `t/grammar-body-my-lexical-scope.t` test 5 was reshaped to stop depending on it.
2. **Slice 2 — bound/token-body scalar provenance (landed 2026-08-22)** (Decision 2 item 2).
   Pin: the `scalar` row of §2.2 as a grammar test (`t/regex-ltm-interpolation-provenance.t`
   probes T and U, the second a `constant` negative control). Watch: `roast/S05-grammar/*`,
   `roast/S05-modifier/my.t`
   (ADR-0022 Slice 5 already had to fix a `:our` fallback that depended on the measurement
   pass leaking a real `env` write — the same class of hidden dependency may exist for
   token bodies) — all stayed green.
3. **Slice 3 — mechanism 1 onto `ltm_branch_rank_key` (landed 2026-08-22)** (Decision 1, the
   easy half; adds litlen). Pin: the `litlen` row of §2.2, in
   `t/regex-ltm-proto-dispatch.t`. Acceptance: `roast/S05-grammar/protoregex.t`
   (whitelisted) stays green — it is the existing regression net for this mechanism.
   - **Implementation note beyond the plan:** `ltm_rank_token_candidate_source`
     parses the candidate and measures it with `ltm_prefix_len_at` /
     `ltm_litlen_at` directly, so it no longer passes through
     `regex_match_with_captures` — and therefore no longer benefits from that
     function's string-level "skip the leading `:my`/`:our`/`:constant`
     declarators while measuring" guard (`regex_match_public.rs`). Measuring a
     `RegexAtom::VarDecl` for real *executes* its initializer, which is exactly
     the ADR-0009 violation that guard existed to prevent. The fix is at the
     atom level instead: a new `LtmAtomMode::SkipZeroWidth` (zero-width, does
     not terminate the prefix, never runs), which is strictly more general — it
     also covers a declarator that is not at the very start of the source text.
     Validated against `raku`: reversing two proto candidates whose only
     difference is a leading `:my` flips the winner, i.e. their prefixes tie, so
     a declarator neither consumes nor terminates.
4. **Slice 4 — mechanism 3 restructured to rank-then-match (landed 2026-08-22)**
   (Decision 1, the semantic
   change). Pin: the `code` / `ws` / `litlen` rows of §2.2 in their nested-`<val>` form,
   plus §2.3's side-effect assertion — all in `t/regex-ltm-proto-dispatch.t`. This is the high-blast-radius slice: every grammar
   in the test suite and every battery (Cro, YAMLish, JSON::Tiny, the vendored Rakudo-Core
   modules) dispatches nested proto tokens through it. Expect a red CI round and fix
   forward; do not split it into smaller gates that leave two rankings live at once.
   - **Left-recursion interaction to watch:** the surrounding `LR_MEMO` / `LR_ACTIVE` /
     `LR_SEED_READ` growing-seed loop currently relies on evaluating *all* candidates each
     iteration to discover whether the seed was consulted. Ranking-then-matching evaluates
     one. The seed-consultation probe must therefore be evaluated over the candidate that
     is actually attempted, and a candidate that is skipped on rank must not be able to
     hide a left-recursive re-entry. Simplest sound rule: keep running the full candidate
     set while `LR_ACTIVE` holds this key, and use rank-then-match only on the
     non-left-recursive path (`!seed_was_consulted`, the overwhelmingly common case).
   - **Residual not closed by this slice:** the `take(1)` on a proto candidate's ends
     (`regex_match_atom.rs:553`) still gives each candidate only its greedy end, so
     backtracking into a *shorter* end of the winning proto candidate is unavailable — the
     proto twin of the gap ADR-0022 Slice 3 closed for `|`. Left as-is; no probe currently
     demands it. Rakudo agrees on the observable half: it does NOT backtrack into a
     lower-ranked proto candidate when what *follows* the subrule call fails
     (`token TOP { <v> 'd' }` over `v:sym<long> { 'abcd' }` / `v:sym<short> { 'abc' }`
     on `"abcd"` answers no-match in `raku`), so "rank, then commit to the first
     candidate that matches" is the correct shape, not a compromise.
   - **Three corrections Slice 4 forced, each validated against `raku` first:**
     (a) `<sym>` is a *named capture* of the literal in Rakudo, not a bare literal
     splice. `instantiate_token_pattern` spliced the escaped text directly, which both
     lost the `$<sym>` capture and — because ADR-0022 §4.3 has a capture group end the
     leading-literal region — handed the candidate a `litlen` tie-break credit Rakudo
     does not give it. It now emits `$<sym>=[…]` (and `[…]` for `<.sym>`), via a scanner
     that skips an occurrence preceded by `$` so the substitution is idempotent.
     (b) A character class written with set **subtraction** (`<[\x1F..\xFF] - [;]>`,
     `<+alpha - [q]>`, `<-[;] - [q]>`) terminates the declarative prefix: Rakudo has no
     single NFA edge for "this set minus that set". Every subtraction-free class
     participates normally. The rule is about the class's *written structure*, not its
     resulting character set, and is independent of any quantifier. **This supersedes
     the root cause the `named-subrule-unbounded-quantifier-wrongly-gets-greedy-ltm-credit`
     ticket recorded** (retired to
     `news/2026-08/subtracted-char-class-terminates-ltm-prefix.md`, which carries the
     corrected rule):
     its "unbounded quantifier reached through a named subrule" theory was wrong — the
     subrule is transparent, and the same shape written inline diverges identically.
     Implementing it fixed that ticket, which Slice 4 had otherwise made *decisive*
     (`t/grammar-body-my-lexical-scope.t` had been reshaped around it and is now
     restored to its strong form).
     (c) `LTM_SEQALT_EPSILON`: the `X || Y` epsilon bypass (§4.2) lets a measurement
     continue past the group at the group's *start* position, so an atom after the group
     is measured against text the real match would have consumed. It can therefore fail
     and drive the whole measurement to `None` even though the candidate matches — which
     made the new `(None, false)` filter drop Cro::Uri's
     `regex host:sym<IPv6address> { '[' <( <.IPv6address> )> ']' }`. A separate
     thread-local now marks such a measurement as unsound to *filter* on without
     truncating the measured prefix the way `LTM_PREFIX_TERMINATED` does. The
     bundled-battery gate caught this; roast did not.
5. **Slice 5 — ledger (landed 2026-08-22)** — retired both originating `todo/deep`
   files to `news/2026-08/proto-token-ltm-one-ranking-mechanism.md` and
   `news/2026-08/subtracted-char-class-terminates-ltm-prefix.md`, and recorded the
   outcome in this ADR's Status line.

## 5. Acceptance matrix

Becomes `t/regex-ltm-interpolation-provenance.t` (Slices 1-2) and
`t/regex-ltm-proto-dispatch.t` (Slices 3-4). Expectations are the raku column of §2.1,
§2.2 and §2.3 above, all verified against system `raku` on 2026-08-20. The headline repro
that opened the investigation:

```raku
grammar G {
    my @opts = <Strict Lax None>;
    token TOP { <name> [';' ' '? <val> ]* }
    token name { <-[;]>+ }
    proto token val {*}
    token val:sym<known> { :i 'Foo=' @opts }
    token val:sym<other> { <-[;]>+ }
}
# ... :actions that `make` "KNOWN" / "OTHER"
say G.parse('x; Foo=Strict', :actions(A.new))<val>[0].made;   # raku: OTHER; mutsu today: KNOWN
```

needed Slice 1 (so `@opts` stops `known`'s prefix at 4) *and* Slice 4 (so the nested
`<val>` dispatch ranks by prefix at all) to flip. Both landed; it answers `OTHER`.

## 6. Consequences

- One ranking algorithm, three call sites: a future LTM refinement is written once.
- ADR-0009's "measuring must not execute" discipline finally reaches nested proto
  dispatch, which is where most grammars actually dispatch protos (§2.3).
- Mechanism 3 gets cheaper, not more expensive (N measurements + 1 real match, versus N
  real matches today).
- `declarative_prefix_match_len` survives only as a fallback for a candidate whose
  source does not parse, plus the "anchored single subrule" ranking loop in
  `regex_match_public.rs` — the last two users of the string-based path.
- No roast test pinned any of §2.2/§2.3 — the acceptance suite was written from these
  probes (`t/regex-ltm-proto-dispatch.t`, 35 assertions, dual-oracle verified). That is
  also why the divergence survived ADR-0009 and ADR-0022: `protoregex.t` reaches only
  mechanism 1.
- **The bundled-battery gate is not optional for LTM work.** Slice 4's `||`-epsilon
  unsoundness passed the full `t/` suite *and* all 1436 whitelisted roast files, and was
  caught only by `scripts/battery-testsuite.sh` (Cro::Core's `uri.rakutest`). Run it
  locally before pushing any ranking change.
