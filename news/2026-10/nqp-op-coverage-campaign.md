# `nqp::` ops: from "on demand only" to a coverage campaign

Since 2026-07 PLAN.md said `nqp::` ops are "added on demand, never as a porting
campaign". The record behind that rule
(`news/2026-07/nqp-op-layer-measured-and-rejected.md`) had already been marked
stale in every number it used, and its central observation — per distribution
the op set is a threshold function, so 80% of a module's ops still leaves it
dead — is really an argument for closing the whole set. The rule is retired:
the documented op set is now implemented category by category, tracked by
[#11488](https://github.com/tokuhirom/mutsu/issues/11488).

To make that measurable, `scripts/nqp-op-coverage.py` builds the inventory. It
reads NQP's own op reference (`docs/ops.markdown`) and the Rakudo-only `p6*`
ops from Rakudo's `Perl6/Ops.nqp`, and probes each op under mutsu as
`use nqp; nqp::<op>(1, ...)` with 0 to 5 arguments. An op counts as
implemented when some arity does not die with `Unsupported nqp:: op`. JS- and
JVM-only ops are out of scope, and so are ops that Rakudo itself rejects
("No registered operation handler"), because no Raku program can reach them.
For example, `nqp::falsey` and `nqp::coerce_sn` exist in NQP but not in the
Raku HLL.

The first run (2026-10-03) found **236 of 586** reachable ops implemented. The
350 missing ones include very basic ops: `nqp::die`, `nqp::sqrt_n`,
`nqp::existspos`, `nqp::iterator`, `nqp::how`, `nqp::findmethod`,
`nqp::getlex` and `nqp::say`. The per-op list is in `docs/nqp-op-coverage.md`.
Each category group has a sub-issue (#11490–#11505). Pure scalar and string
groups are `todo:ticket`. Lexical introspection, the object model,
threads/atomics, serialization contexts and the `p6*` binder ops are
`todo:deep`. Three existing `nqp::` tickets (#11451, #11462, #10956) are linked
as sub-issues too.
