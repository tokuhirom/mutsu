# `last` inside `nqp::while`, and `nqp::while` in a statement-root comma list

Math::Combinatorics' iterators died with "last without loop construct" and, after
that, hung. Two gaps in the `nqp::` loop forms:

- a sunk `nqp::while` / `nqp::until` is a plain jump loop with nothing to catch
  `last` / `next` / `redo`; a body that names one now compiles through the
  ordinary `while` lowering, which registers the loop (bodies without a control
  keep the jump loop);
- an `nqp::while` that is an element of a comma list at statement root was built
  as a lazy Seq nobody pulled, so the loop never ran. Rakudo sinks every element
  of such a list; so does mutsu now.

Pinned by `t/vm/nqp-while-last-and-sunk-list.t`. Math::Combinatorics
`t/combinatorics.t` now matches raku (43 ok).
