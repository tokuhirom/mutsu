# ADR-12577: Closures created by dynamically registered methods preserve captured scalar bindings

- **Status**: Accepted (implemented 2026-10-11)
- **Date**: 2026-10-11
- **Addresses**: [GitHub issue #12577](https://github.com/tokuhirom/mutsu/issues/12577)
- **Related**: [ADR-0092](0092-closure-capture-is-a-chained-tier-not-a-merged-copy.md)
  (capture lookup follows the caller chain before the capture tier)

## Context

A method installed with `.^add_method` can create a closure when called. Its
body is compiled separately from the Method literal, so the ordinary closure
mutation analysis cannot see that the nested closure captures a scalar the
method itself captured. The nested closure then has no shared cell in its
upvalue array. Its by-name fallback resolves through the caller chain before
the capture tier, allowing a same-named lexical in the caller to win.

Issue #12577 reproduces this with a method registered in a loop. Rakudo returns
the loop value (`ca`); mutsu used the caller's value (`czz`). Preferring every
closure's capture in `GetUpvalue` is too broad: some non-cell captures rely on
the live env lookup for writes the analysis does not see
(`t/vm/scope/eval-read-caller-lexicals.t`).

## Decision

When a closure is created inside a dynamically registered method, if it
captures a plain lexical that is also a free variable of the method frame,
promote the method frame's visible scalar capture to a `ContainerRef` cell
before building the closure. The child closure then records that cell as its
upvalue, so its indexed read uses the defining binding and retains later writes.

Only names present in both free-variable sets are considered, and existing
cells are reused. The frame must be a method whose capture marker identifies a
`.^add_method` registration. Other captures retain ADR-0092's lookup
precedence and the by-name fallback used by the evaluation regression above.

## Outcome

The VM closure construction and upvalue capture paths implement the decision.
`t/oo/method/inner-method-captured-loop-lexical-shadowing.t` pins the same-name
caller regression; `t/vm/scope/eval-read-caller-lexicals.t` pins the live
non-cell fallback behavior.
