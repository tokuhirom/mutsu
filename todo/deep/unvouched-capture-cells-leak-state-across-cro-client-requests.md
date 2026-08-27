# Giving every unvouched escaping capture a cell fixes ADR-0055 §1.2(b) but leaks state across Cro::HTTP::Client requests

Measured 2026-08-28 while implementing
[ADR-0055](../../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md)
slice 1. The mechanism below was **implemented, validated, and then removed
again** from the slice-1 PR because of the regression it causes; this file
records exactly what was built and what broke, so it does not have to be
rediscovered.

## What the mechanism is

ADR-0025 slice 2 states the invariant ADR-0055 depends on: *every
escaping-captured plain scalar is either **authoritative** (the creating frame
proves it never changes after capture, so a by-value snapshot is exact) or a
shared `ContainerRef` **cell***. Today neither holds for one population, and
that gap is ADR-0055 §1.2(b).

`needs_cell_locals` is keyed on `captured_mutated_locals`, which only sees a
mutation attributable to a *store by name*. The vouch that produces
`authoritative_free_vars` (the `vouched` set in
`CompiledCode::compute_free_vars`, `src/opcode.rs`) refuses two further shapes:

* an in-place container write (`own_container_writes`), and
* a name handed to a call, where an `is rw` parameter could write it back
  (`own_call_arg_sources ∖ scalar_bind_locals`).

A capture in either shape gets **neither** defence. The fix is one compile-time
set — call it `needs_cell_unvouched_locals` — computed as the exact complement
of `vouched` within the escaping-captured own set, and wired into
`box_captured_lexicals` as an independent trigger alongside
`captured_mutated_locals` / `needs_cell_locals`. That makes the dichotomy
exhaustive by construction.

It works. With it in place:

* ADR-0055 §1.2(b)'s env-resident repro returns `OUTER` instead of `CALLER`
  (`t/closure-capture-cell-dichotomy.t` documents both variants; only the
  slot-resident one is currently pinned).
* The whole `t/` suite (3509 files) and a full local `make roast` (1436 files,
  218836 tests) stay green.
* The #2749 broad-boxing canary does not move: `roast/S32-num/int.t` runs in
  0.06 s.

## What it breaks

The **bundled-library gate** (`scripts/battery-testsuite.sh`, run by CI's `test`
job and NOT by `make test`) drops six whitelisted Cro::HTTP suites:

```
http-auth-webtoken-bearer  http-auth-webtoken-cookie  http-log-file
http-middleware            http-session-inmemory      http-session-persistent
```

The symptom is state leaking between sequential requests on one client. In
`http-middleware` the request path *accumulates*:

```
Server responded with 404 Not Found
  (GET http://localhost:31315/index.SHTML/index.SHTML/counter/echo/)
```

Reproduce with the gate directly:

```
MUTSU_BIN=target/release/mutsu scripts/battery-testsuite.sh
```

or a single file, from `tmp/battery-testsuite/Cro__HTTP`, with the `-I` set that
`batteries.lock`'s `Cro::HTTP` row lists.

## What was already ruled out

The over-boxing was bisected with a temporary `MUTSU_A55_UNVOUCHED` mode switch
over the three vouch-refusal shapes, and the culprit is **not** a shape that can
simply be excluded:

| subset boxed | Cro `http-log-file` | §1.2(b) env-resident |
| --- | --- | --- |
| all (complement of the vouch) | 2 failures | `OUTER` (fixed) |
| none | pass | `CALLER` (bug) |
| `own_call_arg_sources` only | 2 failures | `OUTER` (fixed) |
| `own_container_writes` only | pass | `CALLER` |
| already-`captured_mutated` only | pass | `CALLER` |

So the breaking population is *exactly* the population §1.2(b) needs: a
**read-only** capture of a name that was handed to a call. There is no narrowing
that keeps the fix and drops the regression — the regression has to be
root-caused.

An env-gated trace of the names actually boxed by the new trigger during
`http-log-file.rakutest` is short and points straight at
`Cro::HTTP::Client.request` (`modules/Cro-HTTP/lib/Cro/HTTP/Client.rakumod`
around line 595):

```
6  raw-body-byte-stream
3  url  secure  proxy-url  parsed-url  next-response-promise
3  method  headers-kept  goaway-retries  connection-obtained  broken
2  removed  expected
1  completed
```

`$url` and `$method` are **method parameters**; `$parsed-url`, `$proxy-url` and
the rest are `my` lexicals of `request`, all captured by the big
`Promise(supply { ... })` closure and all handed to calls. `request` also calls
**itself recursively** for redirects (`self.request($method, $parsed-url,
%opts)`), which is the obvious suspect for how one cell ends up shared between
what should be two independent requests.

## Why this is deep, not a ticket

The likely root cause is not in the new trigger but in a freshness gap it
exposes: a slot that already holds a `ContainerRef` is *reused* by
`box_captured_lexicals` (`if self.locals[idx].is_container_ref() { continue }`),
and the reset that gives a redeclaration a fresh binding lives in
`exec_set_local_op`'s vardecl path. A **parameter** binding is not a vardecl, so
nothing in that path resets a stale cell for `$url` / `$method` on the next
invocation — and a recursive call re-entering the same `CompiledCode` makes the
window concrete. Establishing that (rather than assuming it) is the first step:
a `rust-gdb` breakpoint on the boxing site conditioned on the name, plus one on
the parameter-binding store, against the `http-log-file` repro.

Two candidate fixes, both needing design:

1. Never let a **parameter** slot be promoted by this trigger — a parameter is a
   fresh binding created by the caller's argument each invocation, and the
   `is rw`-writeback concern the `own_call_arg_sources` refusal exists for
   applies to a local the frame *declares* and hands to a call, not to the
   frame's own parameter. `CompiledCode` does not currently know its own
   parameter names (`param_name_syms` lives on `CompiledFunction`), so this
   needs a small plumbing change.
2. Make parameter binding reset a stale cell the way a vardecl does, so cell
   reuse can never span two invocations of the same routine.

Neither is a one-liner, and both touch the freshness rules that
`t/for-loop-param-start-sibling-isolation.t` and the loop-body per-iteration
boxing depend on. Hence `todo/deep/`.

## Acceptance

`t/closure-capture-cell-dichotomy.t` grows back its env-resident §1.2(b)
assertion (the ADR §1.2(b) repro with the `my $g = { $b }` forcing line), the
batteries gate stays green, and ADR-0055 §7.4's slice-2 prerequisite list drops
this entry.
