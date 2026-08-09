# Proto-token LTM: an inline unbounded quantified atom outranks an array-interpolated candidate even on a runtime length tie

While root-causing the Cro::HTTP::Cookie `SameSite` cookie-attribute drop (fixed
by making grammar-body `my` lexicals visible to token/subrule dispatch — see
`news/2026-08/grammar-body-my-lexical-scope.md` and
`t/grammar-body-my-lexical-scope.t`), a second, unrelated LTM discrepancy
surfaced that is NOT fixed by that change and is out of scope for it.

## Repro (verified against real `raku`)

```raku
grammar G {
    my @opts = <Strict Lax None>;
    token TOP { <name> [';' ' '? <val> ]* }
    token name { <-[;]>+ }
    proto token val {*}
    token val:sym<known> { :i 'Foo=' @opts }
    token val:sym<other> { <-[;]>+ }
}
class A {
    method val:sym<known>($/) { make "KNOWN" }
    method val:sym<other>($/) { make "OTHER" }
}
my $m = G.parse('x; Foo=Strict', :actions(A.new));
say $m<val>[0].made;   # raku: OTHER   mutsu (post-fix): KNOWN
```

Both candidates match the full remaining text ("Foo=Strict", 10 chars) — a
genuine runtime length tie. Real Raku's LTM still prefers `other` (the bare,
inline, *unbounded* quantified char class `<-[;]>+` written directly in the
token body) over `known` (a literal prefix followed by an array-interpolated
alternation, even though its total declared/real length also reaches 10).

Note the contrast with the case the grammar-body-my-lexical-scope fix DOES
handle correctly: when the catch-all is written as a **named subrule call**
(`token val:sym<other> { <path> }` where `my regex path { <-[;]>+ }`) instead
of the same quantified atom **inlined directly** in the token body, real Raku
flips to preferring `known`. This is exactly the shape of Cro's
`cookie-av:sym<extension> { <path> }` vs `cookie-av:sym<samesite> { :i
'SameSite=' @same-site-opts }`, which now works correctly. Only the *inline*
form (no named subrule) exhibits this further discrepancy.

## Why this is a separate bug from the grammar-body-my-lexical-scope fix

Before that fix, `@opts` silently resolved to an empty array during
interpolation, so `known`'s real pattern was effectively just `:i 'Foo='` (4
chars) — genuinely shorter than `other`'s real match (10 chars). mutsu's
runtime-comparison LTM picked `other` correctly, but only because the lengths
were NOT actually tied (an accidental right answer for the wrong reason). Now
that `@opts` resolves correctly, both candidates truly tie at 10 chars, and
mutsu's tie-break (first-declared-wins, in `src/runtime/dispatch.rs`'s
`eval_token_call_values_at`/`declarative_prefix_match_len`, and separately in
`src/runtime/regex/regex_match_atom.rs`'s subrule-candidate loop) picks
`known` — the first-declared candidate — whereas real Raku picks `other`.

## Suspected root cause (not yet fully confirmed against Rakudo internals)

Rakudo's LTM is not "run every candidate to completion and compare final
match lengths, tie-break by declaration order" — it builds a static NFA from
each candidate's *declarative prefix* and picks by structural longest-token
match, not by re-executing and diffing end positions. An unbounded quantifier
written directly in a token body (`<-[;]>+`, no upper bound) is apparently
treated by that structural analysis as strictly outranking any alternative
whose declared/consumed length is a finite, bounded quantity — even when, for
one particular input, they happen to consume the same number of characters.
A named subrule reference, by contrast, does NOT propagate that "unbounded"
priority tag up to the caller's LTM comparison the same way (matching what
`declarative_prefix_match_len`'s doc comment already says about descending
into subrules to find code atoms, but evidently not about propagating
quantifier-boundedness).

mutsu's `declarative_prefix_match_len` (`src/runtime/regex/regex_resolve.rs`)
and the informally-named "mechanism #2" in
`src/runtime/regex/regex_match_atom.rs` (the `has_proto` branch of the named
subrule-candidate loop) both implement LTM as "execute the real matcher (or a
declarative-mode variant of it) and numerically compare end positions" — a
fundamentally different mechanism than a symbolic bounded-vs-unbounded NFA
priority. Modeling the real Rakudo semantics correctly would need genuine
static analysis of each candidate's quantifier structure (which atoms are
unbounded), which is a materially bigger change than the grammar-body-my fix
and risks being a deep, invasive rework of the whole proto-dispatch engine
(touching both `dispatch.rs` and `regex_match_atom.rs`, which currently use
two independently-evolved LTM approximations — see the note below).

## Why this file, not a same-session fix

- It requires understanding/replicating Rakudo's actual NFA-based LTM
  algorithm (declarative-prefix construction with boundedness-aware
  priority), not just re-injecting a missing lexical.
- mutsu currently has **two separate LTM implementations**
  (`eval_token_call_values_at` for the outermost/`:rule<...>` dispatch vs. the
  proto-candidate loop in `regex_match_atom.rs` for nested `<name>` subrule
  references), which independently reproduce this same divergence. A proper
  fix should likely unify them onto one algorithm rather than patch each one's
  tie-break heuristic separately — that unification is itself a nontrivial
  design question worth an ADR if pursued.
- No known roast test currently depends on this exact shape (inline unbounded
  quantifier competing against an array-interpolated candidate on a tie); it
  was found via exploratory synthetic repros while root-causing the Cro
  cookie bug, not via a failing roast/`t/` test. Recorded here so it is not
  silently lost.

## Suggested next step

If/when picked up: first confirm with `raku --target=ast` or targeted
Rakudo-source reading (not available in this sandbox) exactly how NQP's LTM
weighs unbounded-vs-bounded declared lengths on a tie, then decide whether to
unify `dispatch.rs`'s and `regex_match_atom.rs`'s LTM implementations before
or as part of implementing that priority rule.
