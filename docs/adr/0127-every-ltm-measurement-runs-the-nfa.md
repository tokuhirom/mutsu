# ADR-0127: Every LTM measurement runs the NFA; the walker's measurement mode is retired

- **Status**: Accepted (2026-09-27); implemented in the same PR as this decision
  ([#9644](https://github.com/tokuhirom/mutsu/issues/9644)).
- **Supersedes in part**: [ADR-0022](0022-regex-alternation-ltm-ranking.md) §4.1-§4.2 and
  its implementation notes on the measurement memo (#9579), the recursion stack (#9617)
  and rank reuse (#9617); [ADR-0111](0111-ltm-stoppers-end-one-path.md) §2's description
  of how the *walker* records fates, resets `LTM_PREFIX_TERMINATED`, never scans, and skips
  the ratchet fast paths. What those ADRs decide about *what* the prefix is (what is a
  fate, the furthest fate, the `litlen` tie-break, a fate on a negated multi-alternative
  class or a qualified call) is unchanged.
- **Completes**: [ADR-0125](0125-ltm-declarative-prefix-nfa.md) §6's open item.

## 1. Problem

ADR-0125 compiled a `|` branch's declarative prefix into an NFA, but used it only for a
branch ranked from a real match. Every other measurement still ran the backtracking
matcher under `LTM_DECLARATIVE_MODE`: a proto's candidate ranking, the `:rule<...>` /
outermost proto entry point (`ltm_rank_token_candidate_source`),
`declarative_prefix_match_len`, rankings inside a measurement, and every pattern the NFA
builder declined. So mutsu had two measurement engines, and they disagreed: the walker
honours `:ratchet` while measuring, so `[ \w+ 'c' <.ws> ]? 'ab'` inside a `token` had no
path to the `<.ws>` fate, where Rakudo's NFA (and mutsu's) reaches it at 3. The walker
also needed its own memo, recursion stack, rank reuse, alternation flag scopes and
per-atom measurement branches in the matcher.

## 2. Decision

Every measurement runs the pattern's NFA. The matcher takes part only by answering the
NFA's leaves (one atom at one position), under `LTM_DECLARATIVE_MODE`, which now means only
"nothing reached from here may run user code".

### 2.1 Subrule calls are procedure calls

A `<name>` call compiles to a `Call` node into the callee's body, compiled once per NFA as
a procedure ending in `Return`. The simulation keeps a call stack per thread (interned in
a per-run table), so a thread is a node plus a stack. The recursion cut (Rakudo's `%seen`,
#9617) is decided at run time: a call to a rule already on the thread's stack is a fate.
Inlining (ADR-0125 §2) copied a callee at every call site, and a large grammar blew the
node budget, which had to decline to the walker; a procedure is compiled once, so the NFA
grows with the grammar. A run that makes more than 65536 distinct stacks turns further
calls into fates.

A call whose left-recursion activation is live reads that activation's seed (a
consultation, as a matcher re-entry is), instead of handing the measurement back to the
walker.

### 2.2 The builder never declines

Each construct ADR-0125 §3 declined gets a construction instead:

| Construct | Construction | Rakudo |
|---|---|---|
| `<name(args)>` | a call by name; arguments ignored | inlines by name |
| `<::(…)>` | fate | no name to inline |
| `<&re>`, `<$re>` | fate | a code object called at run time (verified) |
| a plain method of the grammar | fate | a method has no NFA |
| a body whose parse depends on runtime values | `DynCall`: resolved and measured when reached | — |
| `:m` | `Sub`: a nested NFA run over the mark-stripped subject, positions mapped back | — |
| a scoped interpolated regex | `Sub`: a nested NFA run with the scope installed | — |
| `** m..n` past 32 copies | a minimum ends in a fate; a maximum is unbounded | unrolls |
| a `.wrap`ped token, a custom `GrammarHOW` | the body, as usual | the body's NFA |

A rule and a method of the same name: the rule is inlined, as mutsu's matcher prefers it.

### 2.3 What "stopped" means

`ltm_prefix_len_at` returns `(len, stopped)`. `stopped` is true when some path ended in a
fate or went through a `||`. Only `(None, false)` may filter a candidate out: a `None`
with no fate can still be unsound because a `||`'s ε bypass continues at the group's start
(ADR-0046 Slice 4, Cro::Uri's `IPv6address`). Rakudo filters there too (its NFA drops a
candidate whose `||` group blocks the rest); mutsu keeps the candidate for the real match
to judge, as before.

`ltm_rank_token_candidate_source` no longer puts a candidate whose measurement hit a fate
into a "declaration order" bucket: it ranks by where the fate is, as Rakudo does. With
`t:sym<b> { 'ab' }` and `t:sym<a> { 'abc' {} 'd' }`, `G.parse("abcd", :rule<t>)` now tries
`a` (prefix 3) first and parses, where the bucket tried `b` and failed.

### 2.4 A single non-proto candidate always runs

The `:rule<...>` entry point used to skip the real match of a single non-proto candidate
whose measurement said it cannot match, except when actions or `$*HIGHWATER` made the
failed match observable. Rakudo runs no NFA for a plain rule call, and a `~` goal missing
inside it must reach `FAILGOAL` (the walker's measurement used to record that goal as a
side effect). The single candidate now always runs and fails for real.

## 3. What was removed

`regex_ltm_memo.rs`, `regex_ltm_recursion.rs`, `regex_ltm_rank_reuse.rs`,
`LtmAlternativeScope`, `LTM_PREFIX_TERMINATED`, `LTM_SEQALT_EPSILON`,
`ltm_seqalt_candidates` / `ltm_seqalt_best`, the proto "union of every end under
measurement" branch, the measurement exemptions from `:ratchet` and the quantifier
shortcuts, the quantifier DFS's measurement `seen` set, and `MUTSU_LTM_NFA_VERIFY` (there
is no second engine to compare with).

`LTM_DECLARATIVE_MODE` remains, set only by an NFA run around its leaves. The atom
matchers' guards under it (a non-declarative atom is a fate, a code atom is not run, a
`:my` initializer, a `$*` rule frame, a `.wrap`per and the action replay log are skipped)
cover a match nested in a leaf — a `<+name>` class calling a token, say.

## 4. Consequences

- Protos, `:rule<...>` and nested rankings measure what Rakudo's NFA measures, including
  the paths `:ratchet` would cut (`t/regex/regex-ltm-nfa-entry-points.t`).
- The measurement no longer has side effects the rest of the engine relied on (§2.4).
- Known remaining differences from Rakudo: `||` (above), the bounded `** m..n` of
  [#9637](https://github.com/tokuhirom/mutsu/issues/9637), and `:i` with a
  multi-character case fold, whose leaves compare one character at a time.
