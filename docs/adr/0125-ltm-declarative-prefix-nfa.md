# ADR-0125: Measure an LTM declarative prefix with a compiled NFA

- **Status**: Accepted (2026-09-26, user decision). Phase 1 is implemented in the same PR
  as this decision (see §6).
- **Amends**: [ADR-0022](0022-regex-alternation-ltm-ranking.md) §4.1, *how* a branch's
  `prefix_len` is computed. What counts as a fate, the recursion cut
  ([#9617](https://github.com/tokuhirom/mutsu/issues/9617)), the `litlen` tie-break and
  ADR-0111's "a fate ends one path" are unchanged.
- **Context**: #9617 — `regex A { '{' [ <A> | . ]*? '}' }` parses about 45 times slower
  than Rakudo, after #9616, #9618 and #9625 had already brought it from exponential to
  quadratic, which is Rakudo's own order on this shape.

## 1. Problem

mutsu measures a declarative prefix by running the ordinary backtracking matcher under
`LTM_DECLARATIVE_MODE`. The result is the furthest end or fate over every path. The matcher
builds what a real match needs and a measurement does not: capture stores and their deltas,
`RegexCaptures` clones, a continuation per candidate, and an allocation for each set of
ends. On the #9617 repro almost all of the parse is measurement, about 10,000 instructions
per subject character a measurement walks, and the cost is spread thin over the whole
matcher. No local fix removes it. #9625 removed the one large redundancy, a second walk per
nested `|` branch, and that bought 23%.

Rakudo compiles each rule's declarative prefix into an NFA once, and ranks by running the
NFA over the subject. A step of that simulation is a set of states and a character test.

## 2. Decision

Compile a `|` branch's declarative prefix into an NFA once, cache it on the pattern, and
simulate it over the subject to get the branch's `prefix_len`.

- **Construction** follows ADR-0022 §2's table as the walker implements it:
  - sequences, groups, capture groups and `|` compile to NFA structure;
  - `||` compiles to its first branch plus an ε bypass (ADR-0022 §4.2);
  - quantifiers compile to loops and splits. `**` ranges and `%`/`%%` separators are
    unrolled up to a small bound;
  - a positive lookahead compiles to its inner pattern, whose accept is a fate;
  - everything `ltm_atom_mode` calls `Terminate` becomes a fate node, as do a plain
    `{ }` block, a `** {code}` quantifier and a runtime-interpolated literal;
  - `<?{ }>`, `:my` and capture markers are ε.
- **Subrules are inlined**, each body compiled in its defining package, with the inherited
  `:i` applied at the body's top level exactly as `subrule_candidate_ends` does. A call to a
  rule already being inlined on the current path becomes a fate: this is Rakudo's `%seen`,
  and the walker's `regex_ltm_recursion` cut. Resolution goes through the same
  `(pkg, TOKEN_DEFS_GEN)`-keyed table the matcher uses, under the same soundness conditions
  ADR-0099 §4 puts on the prefilter (`regex_prefilter_subrule`).
- **Leaves keep one implementation.** A character-consuming or zero-width atom is not
  re-implemented in the NFA. A leaf node calls the existing matcher for that one atom at
  that one position, under `LTM_DECLARATIVE_MODE`, inside the measurement's fate frame:
  - the single-end prober (`regex_match_atom_in_pkg`) for atoms with at most one end;
  - the plural atom matcher for a builtin `<name>` (a name no rule answers to).
- **The cache** lives on `PatternDerived`, keyed by `(package, TOKEN_DEFS_GEN)`, like the
  subrule-aware prefilter. A pattern the builder declines is cached as declined, so it is
  not rebuilt on every ranking.
- **Where it is used.** Phase 1 uses the NFA only in `ltm_branch_rank_key`, and only for a
  measurement started from a real match: not already measuring, no live left-recursion
  activation, no `.wrap`ped token, no custom `GrammarHOW`. `ltm_branch_rank_key` ignores
  the "stopped" flag, so the NFA does not have to reproduce the walker's
  `LtmAlternativeScope` flag rules. Every other entry point stays on the walker.

## 3. What the builder declines

A declined pattern is measured by the walker, exactly as before. The builder declines:

- a subrule with arguments, `<::(…)>`, a name that may name a lexical `Regex` (`<&…>`), or
  a body that is not generation-stable (its text interpolates a runtime value);
- a name that both a rule and a method answer to;
- a proto (a multi-candidate rule with `sym` keys). The walker ranks the candidates and
  keeps only the winner's greedy end (ADR-0046 §4's residual); reproducing that in an NFA
  is not a structural question, and changing it to Rakudo's full union is a ranking change
  of its own;
- `:m` (ignoremark), a scoped `CaptureIsolatedGroupScoped`, and an NFA over the node
  budget.

## 4. Where the NFA and the walker may differ

The NFA explores every path. The walker has two shortcuts that can make it explore fewer:

- a quantifier over an atom without an alternation (`walk_quant_chain`) takes each
  iteration's first end only;
- `QUANT_ALT_BUDGET` caps a quantifier's DFS.

Where the walker's shortcut missed a path, the NFA's prefix is the longer one, and it is
the one Rakudo's NFA computes. `MUTSU_LTM_NFA_VERIFY=1` runs both on every NFA ranking and
reports each difference on stderr, so a difference is found by running a suite, not by
reading code.

## 5. Rejected alternatives

- **Keep optimizing the walker** (a capture-free measurement mode, fewer allocations).
  Profiles put capture handling and allocation at about half the cost, so the best case is
  about 2×, against a 45× gap. It also leaves a dual-purpose matcher, in which every real
  match feature has to be taught what to do under measurement.
- **A persistent memo across rankings.** The rankings in the repro start at different
  positions and share no measurement, so a memo does not reduce the work.
- **Cutting the prefix earlier than Rakudo** (at the first subrule call). That changes
  rankings, and Rakudo does not do it.

## 6. Implementation status

- Phase 1 (this PR): the NFA in `src/runtime/regex/regex_ltm_nfa*.rs`, used by
  `ltm_branch_rank_key` for measurements started from a real match, plus
  `MUTSU_LTM_NFA_VERIFY`.
- Open:
  - protos (§3);
  - the other measurement entry points: `ltm_prefix_len_at`'s other callers need the
    "stopped" flag;
  - retiring the walker's measurement mode once every entry point is on the NFA.
