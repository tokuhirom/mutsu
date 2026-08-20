# Named-subrule call with an unbounded quantifier and no internal stopper wrongly gets greedy LTM credit

## Root cause

Rakudo's LTM declarative-prefix ranking treats a bare named-subrule call
(`<name>`) as an opaque "fate" arc for its own **unbounded** content: if the
subrule's body reaches an explicit stopper (a code atom, `<.ws>`, a
backreference, ...) before running out of fixed-width material, the prefix
contributed is the fixed-width reach up to that stopper (this part already
works in mutsu — see `nested_subrule_sees_terminator_through_recursion` in
`src/runtime/regex/regex_ltm_rank.rs`). But if the subrule's body has **no**
stopper and ends in an unbounded quantifier (`+`, `*`, `**N..*`, ...), Rakudo
does **not** give the calling candidate credit for a real/greedy match through
that quantifier — the candidate is ranked as though it barely matched at all,
and loses LTM ranking to a sibling with a shorter but genuinely fixed
declarative prefix.

This is a real, general Rakudo semantic — not a quirk of proto dispatch
specifically — but the SAME shape reached *directly* (not through a named
subrule) *does* get full/greedy real-match credit. So the distinguishing
factor is specifically "was this content reached via a named-subrule call",
not "is this content declaratively unbounded".

mutsu's `ltm_atom_mode` (`src/runtime/regex/regex_ltm_rank.rs`) classifies
`RegexAtom::Named` as `Normal` (i.e. "measure exactly as a real match would")
for everything except a `ws`-named lookup. Under `LTM_DECLARATIVE_MODE`, the
named-subrule dispatch loop (`regex_match_atom.rs`, the `has_proto`/candidates
loop around line 541) calls `regex_match_ends_from_caps_in_pkg` on the
subrule's own pattern for real, which happily runs the unbounded quantifier to
completion (greedy, against the real subject text) when the subrule's body has
no stopper to trip `LTM_PREFIX_TERMINATED`. This over-credits the calling
candidate relative to Rakudo.

## Minimal repro (raku-verified, no array/regex-object interpolation involved
at all — fully independent of ADR-0046)

```raku
my regex catchall { <[\x1F..\xFF] - [;]>+ }
say "Foobar" ~~ / <catchall> | 'Foo' /;   # raku: Foo   mutsu: Foobar
```

Contrast with a bare (non-subrule) unbounded quantifier, which correctly gets
greedy credit in both engines:

```raku
say "Foobar" ~~ / \w+ | 'Foo' /;          # raku: Foobar  mutsu: Foobar (agrees)
```

And contrast with a subrule whose body *does* have an internal stopper, where
recursing in and reporting the partial (stopper-bounded) prefix is correct in
both engines (mutsu already gets this right, pinned by
`nested_subrule_sees_terminator_through_recursion`):

```raku
grammar G {
    token boundary { ab <.ws> c }
    proto token val {*}
    token val:sym<other> { <boundary> }
    token val:sym<lit>   { 'a' }
}
# G.subparse('ab   c', :rule<val>) picks `other` (boundary's own prefix 'ab'
# = 2 chars, beats lit's 1 char) — both engines agree.
```

## Why this is a design-needed (`todo/deep`) item, not a quick ticket

Fixing it soundly needs the named-subrule dispatch loop
(`regex_match_atom.rs`, currently the "mechanism 3" candidate loop ADR-0046
calls out — see `docs/adr/0046-proto-token-ltm-shares-one-ranking-mechanism.md`)
to distinguish "I am recursing into a *called* subrule's own declarative
measurement" from "I am measuring the candidate's own top-level body",
probably via a new thread-local (mirroring `LTM_DECLARATIVE_MODE` /
`LTM_PREFIX_TERMINATED`) consulted by whatever code loops an unbounded
quantifier's repetitions, so that loop can terminate at the first repetition
when running *inside* a subrule-call recursion but keep running greedily at
the top level. That is exactly the kind of shared-primitive surgery
ADR-0046's Decision 1 (one ranking mechanism, three call sites) is meant to
land in one place — this finding should be folded into that work (Slice 3/4)
rather than patched in isolation, since the same dispatch loop is what Slice 4
restructures.

## How this was discovered

Surfaced while implementing ADR-0046 Slice 1 (array/regex-object interpolation
provenance): correctly narrowing `token val:sym<known> { :i 'Foo=' @opts }`'s
own measured declarative prefix (from a wrongly-inflated ~10 chars down to the
correct 4, `'Foo='`) exposed that `t/grammar-body-my-lexical-scope.t`'s test 5
(`GBMLS-Proto.parse('Foo=Strict', :rule<val>, ...)`, where the sibling
candidate `token val:sym<other> { <gbmls-path> }` calls a named subrule with
no internal stopper) was passing on `main` only by *coincidence*: both
candidates' measured prefixes were wrong in the same direction (known's
`@opts` inflation, other's subrule-recursion over-credit) and happened to
still rank in the right relative order. Slice 1 fixed the first bug, which
made the second, pre-existing, independent bug newly decide the outcome.
`t/grammar-body-my-lexical-scope.t`'s test 5 was rewritten (in the same PR
that filed this ticket) to verify `:rule<...>` direct dispatch resolves a
grammar-body `my` array using a non-competing single-candidate grammar
(`GBMLS-Basic.parse(..., :rule<TOP>)`) instead, so it no longer depends on
this unrelated, unfixed gap. The named-subrule-vs-array-interpolation LTM
race itself (tests 1-4 in that file, reached through the *nested* `<val>`
dispatch mechanism — ADR-0046's "mechanism 3") is untouched by Slice 1 and
still passes, by design (ADR-0046 §2.2: mechanism 3 stays red/unaffected until
Slice 4).
