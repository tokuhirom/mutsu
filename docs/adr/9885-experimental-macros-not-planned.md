# ADR-9885: The experimental `macro` / `quasi` feature is not planned — Rakudo removed it

- **Status**: Accepted (2026-10-11). Supersedes Phase 6 of
  [ADR-0011](0011-rakuast-model-layer-and-phasing.md); the rest of ADR-0011 is unaffected.
- **Date**: 2026-10-11
- **Deciders**: tokuhirom, Claude
- **Issue**: [#9885](https://github.com/tokuhirom/mutsu/issues/9885) (closed as not planned)
- **Related**: [ADR-10723](10723-rakuast-is-the-frontend-ir.md) §2.3, which drops the same phase
  as part of its (still Proposed) frontend decision. This ADR records the macro decision on its
  own so it does not wait on that one.

## Context

ADR-0011 planned a Phase 6: `macro` declarations, `quasi { … }` AST quoting, `{{{ … }}}`
unquoting and splicing (with `X::TypeCheck::Splice`), built on the RakuAST construction and
`EVAL` phases. It was never started. #9885 gave it a queue entry after the 2026-09-27 doc-diff
sweep found the macro examples in `Language/experimental.rakudoc` and
`Type/X/TypeCheck/Splice.rakudoc` failing under mutsu.

Upstream has since removed the feature it describes:

- After a Steering Council decision ([Raku/problem-solving#520](https://github.com/Raku/problem-solving/issues/520)),
  Rakudo disabled the experimental macros in 6.e (2026-06-14, "Disable macros in 6.e"). The
  stated reason is that nobody was satisfied with the experimental macro syntax or semantics, and
  real macros are to be designed on top of RakuAST.
- Raku/doc commit `9f9bba5` (2026-06-14) added this note to both doc pages: "**Experimental**
  macros will not be available in language version 6.e, to clear the way for actual macros
  using RakuAST in 6.f." The examples below the note are still there, which is why the doc-diff
  harness reported them.
- Rakudo 2026.09 goes further than the note. Every form below fails to compile, under the
  default 6.d, under `use v6.e.PREVIEW`, and under `RAKUDO_LEGACY=1`:

  ```
  $ raku -e 'use experimental :macros; macro m() { quasi { "x" } }; say m()'
  Error while importing from 'experimental': no such tag 'macros'
  $ raku -e 'macro m() { quasi { "x" } }; say m()'
  Unexpected block in infix position (missing statement control word before the expression? …)
  ```

So the reference oracle no longer runs any program that #9885's acceptance criteria named. The
issue's "raku: `x`" repro output was not reproducible when it was filed.

## Decision

1. mutsu does **not** implement the experimental `macro` / `quasi` / `{{{ … }}}` feature.
   ADR-0011 Phase 6 is withdrawn, not deferred.
2. mutsu matches current Rakudo by rejecting it. `use experimental :macros` and the `macro`
   declarator are not recognized. No compatibility shim is added for code written against the
   old feature.
3. RakuAST-based macros, which Rakudo has announced for 6.f, are a separate future feature. When
   Rakudo ships a design for them, file a new issue and write a new ADR from that design, not from
   the old Phase 6 plan or from `old-design-docs/` (S06's macro section describes the feature
   that was removed).
4. The doc-diff divergences at `Language/experimental:66,78,93,104` and
   `Type/X/TypeCheck/Splice:30` are triaged as "upstream doc still shows a removed feature". They
   are not mutsu findings.

## Consequences

- #9885 is closed as not planned. No `t/` pins are added for macro behavior, because there is no
  oracle behavior to pin.
- The RakuAST work (ADR-0011's phases 1–5 and ADR-10723) has no macro-shaped goal left.
- If a future Raku/doc re-vendor drops the macro examples, the triage line in
  `docs/doc-diff-backlog.md` can be removed.
