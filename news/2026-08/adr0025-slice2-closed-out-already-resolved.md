# ADR-0025 slice 2 closed out: already resolved by intervening work

`todo/deep/adr0025-slice2-implementation-plan.md` (written 2026-08-11) laid
out a step-by-step plan to implement ADR-0025 slice 2 — making captured
plain-scalar cell boxing unconditional for every "vouch-refused" (captured
and mutated, not proven safe to snapshot by value) local, so the closure
escape verdict stopped being a correctness gate and became a pure perf hint.
It was picked up for direct implementation on 2026-08-20; per this project's
"things go stale fast" triage rule, the plan's premises were re-verified
against `main` before writing any code, and every one of them turned out to
already be fixed.

## Step 0: the cross-thread race is gone

The plan's Step 0 pointed at a specific, previously-flaky repro:
`http2-response-serializer.rakutest` "check 4" failed on roughly half of 8
runs on 2026-08-11 (a race in the closure-dispatch captured-env merge, where
a stale plain value could beat or replace a `ContainerRef` cell on the
worker-thread side of a cross-thread dispatch). Cro is bundled as a battery
(`docs/batteries/cro-http.md`), so the exact upstream test suite could be run
directly against the built `mutsu` via the `-I` paths recorded by a prior
session's battery fetch. Re-run 8 times on the debug binary: **0/8
failures**, and all 29 subtests pass on every run, not just check 4. The
sibling suites the plan's residuals section named
(`http2-request-parser.rakutest` 61/61, `http2-request-serializer.rakutest`
32/32, `http2-response-parser.rakutest` 9/9) are also fully green.

The most likely fix is `2011b083b` (2026-08-19, "reuse the source cell for
SetGlobal `:=` binds and stop dropping cell promotions across nested call
frames"), which touched exactly the closure-dispatch captured-env merge
(`vm_closure_dispatch.rs`) the plan's Step 0 pointed a `rust-gdb` session at
— landed for an unrelated symptom (`t/has-attr-binding.t` test 6) that shared
the same merge-site defect class.

## Steps 1-4: the "escape verdict" gap is already closed

The plan's motivating shapes — a closure stored via `@registry.push($cb)`,
`.tap($cb)`, and a constructor named-arg (`Holder.new(now => $cb)`), each
tried both as a pre-bound variable and as a literal written directly at the
call site — all correctly read the creator's post-capture rebind on `main`.
So do shapes the plan didn't name explicitly: a plain (non-method) function
call passed a stored closure variable, and a closure literal assigned
directly into an array/hash *element* rather than a `$`-named `my`.

Root cause: the plan predates two changes that together close the gap it was
designed to fix.

1. `cf9dc72be` (2026-08-04, "a closure passed as an argument shares its
   captured container") made `method_escapes_closure_args` unconditionally
   `true` — every closure argument to a method call escapes now, not just
   the old `then`/`tap`/`act`/`start` allowlist.
2. The compiler's pre-existing `escaping_position` flag already covers
   assignment/`VarDecl` RHS, `return`, bind RHS, and literal collection
   elements — together with (1), every syntactic position through which a
   closure could become reachable after its creating frame returns is
   already treated as escaping, forcing the shared `ContainerRef` cell the
   plan wanted to make unconditional.

What remains classified non-escaping — control-flow bodies, `sort`/`map`/
`grep` predicate blocks — is correctly non-escaping: those blocks run
synchronously inside the call that created them and are never stored, so
there is no window in which staleness could appear.

## No code change, existing pins stay green

`t/closure-capture-instance-cell.t`, `t/for-loop-param-start-sibling-isolation.t`,
`t/named-sub-lexical-scope.t`, `t/lock-protect-shared-scalar.t`,
`t/closure-container-capture-alias.t`, and
`t/closure-arg-shares-its-captured-container.t` all stayed green throughout
this investigation (54 tests, 6 files). Since no new mechanism was added, no
new pin was needed — reproducing any of the fixed shapes above is already
covered by whichever intervening commit closed it. ADR-0025 was updated with
a "Slice 2 outcome" section recording this closeout; slice 3 (type/`where`-
constrained scalars, `$`-held Array/Hash, `@`/`%`/`&` rebinding) remains open
and unchanged by this finding.
