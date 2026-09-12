# The batteries adoption policy is an ADR now, and it says what a benchmark cannot buy

The rule the whole batteries effort rests on — **vendor the upstream module
unchanged and grow the interpreter until it runs (rung 2); native provision
(rung 3) is banned** — was a user decision from 2026-08-01 that lived only as
prose in `BATTERIES.md` §1 and `CLAUDE.md`. Everything downstream cited it;
nothing held its reasoning. It is now
[ADR-0096](../../docs/adr/0096-batteries-adoption-policy.md) (#8184).

Prose was enough to state the rule and not enough to defend it. Three things had
no home, and the gap between them produced a live carve-out.

## The clause that was missing

**A performance measurement justifies an optimization, never a substitution.**

- An *optimization* is selected transparently and is semantically
  indistinguishable from the code it replaces — what the JIT does to bytecode.
- A *substitution* changes what the program observes: a different exception
  type, a different message, a different edge-case answer, a different module
  resolving under the same name. It is banned, and no benchmark buys an
  exemption.

Rung 3 is banned because a module that only looks like the upstream one is a
private dialect. That argument does not weaken when the divergence is bought
with speed instead of with convenience — and leaving "unless it is slow"
implicit is exactly what let the JSON interception in.

The ADR names one mechanism on each side of the line. The parse-time
`TEST_EXPORTS` list is legitimate: it caches an export-name set that keeps
`use Test` at 7ms instead of 93ms, the real `Test.rakumod` still runs, and
`test_exports_match_the_vendored_module` fails the build the moment the cache
disagrees with the vendored source. It cannot diverge observably, and a test
says so. The JSON path is not: it selects a different implementation by module
name, and picks its exception type from the *set* of loaded module names, so a
program that loads both `JSON::Fast` and `JSON::Tiny` gets `JSON::Fast`-shaped
errors from its `JSON::Tiny` calls.

## The exception list is now auditable, and one entry is not justified

Two entries, and the ADR requires each to state what makes rung 2 unreachable
and what would reopen it:

- **`NativeCall`** (#7560) — justified. The verdict rests on structural
  blockers, not on op count: `use QAST:from<NQP>`, and a `Dispatcher` written
  against MoarVM's dispatch programs. Its own reopening condition is on record.
- **The JSON `to-json`/`from-json` fast path** — an exception scheduled for
  retirement (#8183), *not* policy. `docs/batteries/json-tiny.md` had declared
  the split permanent; that claim is withdrawn in this change. A battery's
  selection record documents a selection and its provenance — it does not get to
  make a policy decision, and this one was doing so on a rationale that had
  since aged out on both halves.

Retirement is the expected end state, and the ADR records what it costs in
practice: `Pod::To::Text` (#5644) came back to the real rakudo module for three
general interpreter fixes, and the native `Test` provider was deleted outright
on 2026-09-10 — ~3,300 lines, **0 regressions** across 3,950 `t/` files and
1,436 whitelisted roast files.

## "Do not build an `nqp::` op layer" is not what was measured

`news/2026-07/nqp-op-layer-measured-and-rejected.md` keeps being cited in that
blanket form against work that is already happening: mutsu has **111 `nqp::` ops
across ~1,790 lines** and is still growing them. The durable finding is narrower
and still true, and the ADR carries that version:

> The op set a module requires is a **threshold function**. Implementing 80% of
> a module's ops leaves it dead, so a *large* module is not reached by adding
> ops one at a time.

Which is why the same measurement's own worked example was `nqp::sha1` — one op,
serving zef's `Distribution.id` and bundled OpenSSL — while the "obvious" 42-op
`JSON::Fast` target would have unlocked nothing. Ops are worth implementing when
they serve something mutsu already ships; grinding a large module's op list one
op at a time is the shape that was rejected.

## What changed on disk

Documentation only. `docs/adr/0096-batteries-adoption-policy.md` plus the
references that were pointing at a policy with no document behind it:
`BATTERIES.md` §1 (the ADR link and the new performance clause), `CLAUDE.md`'s
ban clause (which also still listed the deleted native `Test` provider among the
exceptions), `PLAN.md`, `docs/adr/README.md`, `docs/batteries/json-tiny.md`
(the withdrawal), and `ANALYSIS.md` — whose §8 "One ADR is still missing" no
longer is.
