# The subrule atom answers its own questions

Round 20 of [#7576](https://github.com/tokuhirom/mutsu/issues/7576). Instructions
on the 60-row YAMLish document go **1,277,410,693 -> 1,239,208,554 (-2.99%)**.
Attribution is callgrind `Ir` throughout, cache warmed first.

Both items are the same shape: a question whose answer was already fixed before
the matcher started, asked again on every atom match.

## (a) 402,202 calls to arm a seed that nothing reads (-1.63%)

Every atom match calls `arm_inline_vars_seed`, which publishes two things to the
nested walks the atom is about to run: the enclosing regex's `:my`/`:let`
lexicals, and the enclosing level's captures for a backreference to resolve
against. Both are real mechanisms with real tests behind them — and on this
workload both are inert. The function ran **402,202 times and touched a
thread-local in 507 of them**.

It still cost 1.63% of the whole program: 20.2 M `Ir` in the call itself (a
seven-way `matches!` over the atom, two seed constructions, an out-of-line call
and return) plus 6.0 M more in `drop_in_place<(InlineVarsSeed, OuterCapsSeed)>`,
the out-of-line drop glue for two guards that each immediately found `armed:
false`.

Neither "is anything published" question needs to look at the atom at all:
`any_regex_backref_lowered()` is a process-global `AtomicBool` the parser sets
the first time it lowers a backreference anywhere, and "is a lexical published"
is a thread-local `Cell<bool>` plus `current_caps.regex_vars_shared()`. When all
three say no, whichever branch the function would have taken arms
`InlineVarsSeed::arm(None)` against an empty slot (inert) and
`OuterCapsSeed::inert()` — so the fast path returns exactly the pair the slow
path would have built. It is `#[inline]` now, with the real work behind
`#[inline(never)] arm_inline_vars_seed_cold`, and the two `Drop` impls are
`#[inline]` so the inert case leaves no drop glue to call. In the profile the
function disappears and the cold path is left at 40,597 `Ir`.

The cold path is unchanged and stays pinned by
`t/regex/regex-backref-in-group.t` (the capture-scope rules, all fourteen
verified against raku) and `t/regex/regex-my-var-in-subpattern.t` (a `:my`
lexical reaching every flavour of inline sub-pattern).

## (b) A `<subrule>` atom's lookup spec, re-derived per call (-1.35%)

`parse_named_regex_lookup_spec` turns the text between the angle brackets into
the spec the matcher acts on: silent or not, token-lookup or not, the alias and
whether it replaces the original, the interned lookup and capture names, the
argument expressions. It is a pure function of the text, and the text is fixed
once the pattern is parsed. It was memoized — on a process-wide
`FxHashMap<String, Arc<NamedRegexLookupSpec>>`, probed **113,590 times** on this
parse for 17.2 M `Ir`, every probe hashing the same atom text again to hand back
the same `Arc`.

`RegexAtom::Named` carries a `NamedAtom` now: the text, plus a `OnceLock` for
the spec derived from it. The callers all hold that node — they matched on it to
get the `&str` they were passing — so they ask it directly. The first ask still
goes through the global memo, so two atoms spelled the same still share one
`Arc` exactly as before; every later one is an initialized `OnceLock` read. The
compiler enumerated the change: 16 construction sites take `.into()`, and 14 of
the 18 spec lookups became `name.spec()` where the node was in hand (the four that
work from a runtime-built string — `<::(EXPR)>` indirection, an interpolated
pattern, the call-graph walk's memo miss — keep the global entry point).

Pinned by `t/regex/regex-named-atom-spec-forms.t`: one of each atom shape the
spec distinguishes (plain, `<.silent>`, `<al=word>` under both names,
`<dot=.word>`, `<rule(args)>`, a quantified subrule) plus two grammars whose
identical atom text must resolve to their own rules — the spec is shared per
text, the resolution is not. Verified against rakudo 2026.07.

## Where the next round starts

Re-measured after this round, not carried forward from round 19 (see round 12's
note — round 19's own winner was not on round 18's list):

- **Left-recursion bookkeeping is now the largest single cluster, ~3.6%.** The
  two biggest remaining thread-local clients are `for_each_atom_candidate`
  (22.2 M, 134,850 calls) and `regex_match_atom_all_with_capture_in_pkg_inner`
  (22.0 M, 130,045), and in both it is the inlined `lr_begin_or_reenter` /
  `lr_seed_was_consulted` / `lr_end_activation` trio — three hash-map operations
  per `<subrule>` call on a grammar with no left recursion at all. The gate
  (`regex_call_graph::reenter_decline`) already exists; what makes it not a
  slice is that `LrKey` carries no package while the analysis is per-package,
  and a wrong `None` fails as unbounded recursion rather than a wrong answer.
- `Symbol::intern` at 2.34% (209,401 interns, diffuse callers).
- `subrule_call_stream_decline` at 0.61% — 46,677 probes of a
  `(pkg, atom text)` memo, now that the atom is a node that could hold the
  verdict itself; needs a `Sync` cache, since the verdict is keyed by token
  generation and package rather than once-only.
- The capture store (`CapStore::merge_delta` 1.79% self, `RegexCaptures::clone`,
  `rewind`) is unchanged and still wants a design pass rather than a slice.

Cumulative over rounds 11-20 on the 60-row document: **8.130 Bn -> 1.239 Bn `Ir`
(-84.8%)**.
