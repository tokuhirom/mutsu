# The Cro::HTTP::Client state leak was a parameter being given a shared cell

Filed 2026-08-28 as `todo/deep/unvouched-capture-cells-leak-state-across-cro-client-requests.md`,
resolved 2026-09-06 while landing ADR-0055 slice 1b
(`news/2026-09/adr0055-unvouched-escaping-captures-get-a-cell.md`). The ticket's
own leading hypothesis was right, and its resolution cost one line.

## What the ticket recorded

ADR-0025 slice 2's invariant — *every escaping-captured plain scalar is either
authoritative or a shared `ContainerRef` cell* — was not exhaustive. The August
investigation built the missing half (`needs_cell_unvouched_locals`, the exact
complement of the vouch within the escaping-captured set), confirmed it fixed
ADR-0055 §1.2(b) outright, confirmed `make test` and a full local `make roast`
stayed green and that the #2749 broad-boxing canary did not move — and then
**removed it from the shipped slice**, because it dropped six whitelisted
Cro::HTTP suites in the bundled-library gate (a CI step `make test` does not
run). State leaked between sequential requests on one client, visible as an
accumulating request path:

```
Server responded with 404 Not Found
  (GET http://localhost:31315/index.SHTML/index.SHTML/counter/echo/)
```

A three-way bisect over the three vouch-refusal shapes showed the breaking
population was *precisely* the read-only call-arg-source population §1.2(b)
needs, so there was no narrowing along that axis that kept the fix. The ticket
concluded the regression had to be root-caused, and offered two candidate fixes.

## What it actually was

Candidate 1. The boxed-name trace already pointed at it: `$url` and `$method`
are **method parameters** of `Cro::HTTP::Client.request`, which also calls itself
recursively for redirects.

A parameter is a fresh binding the *caller* creates on every invocation. The
`is rw`-writeback hazard that makes `compute_free_vars` refuse to vouch for a
name in `own_call_arg_sources` is about a local the frame **declares** and then
hands onward — it does not apply to the frame's own parameter. Meanwhile a `my`
redeclaration clears a stale cell and starts a fresh binding
(`exec_set_local_op_inner`'s `is_vardecl` arm), and a parameter binding is not a
vardecl, so nothing reset the cell between two invocations of one routine. Two
requests therefore shared one binding.

So `needs_cell_unvouched_locals` filters out `CompiledCode::param_locals`. That
field is new: `CompiledCode` did not know its own parameter names (the ticket
noted this — `param_name_syms` lives on `CompiledFunction` and covers positionals
only), so `Compiler::declare_param`, the single entry point for parameter
declaration, now records them. The batteries gate is back to its whitelisted
count with the mechanism in.

## The narrow shape this trades away

A capture of a parameter that was *itself* handed to a call
(`sub outer($p) { noop($p); { $p } }`) now has neither defence, and is
hijackable by a same-named lexical in the calling frame. Recorded, with the
measured repro and the shape of the real fix (the ticket's candidate 2 — make
parameter binding reset a stale cell the way a vardecl does), in
`todo/tickets/parameter-capture-handed-to-a-call-has-neither-defence.md`.

## Acceptance, as stated in the original ticket

`t/closure-capture-cell-dichotomy.t` grew back its env-resident §1.2(b)
assertion — four of them, one per invocation path — plus a parameter-freshness
guard that pins this regression directly. The batteries gate stays green, and
ADR-0055 §7.4's slice-2 prerequisite list drops this entry.
