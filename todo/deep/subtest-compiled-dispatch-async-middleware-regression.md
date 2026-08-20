# Compiled-first `subtest` dispatch breaks Cro::HTTP middleware — root cause unknown

## Status (re-verified 2026-08-20 against `main` at 227e38e4f)

Still open, and **worse than originally recorded**. The escaped-class-registration
half of the original finding has been split out: it is a genuine architectural
problem in its own right and is now designed in
[ADR-0047](../../docs/adr/0047-type-identity-is-a-declaration-site-not-a-registry-name.md)
("A type's identity is its declaration site, not its current registry name").
This file keeps only the part that ADR-0047 does **not** explain: why the
compiled dispatch path breaks Cro::HTTP's middleware suite.

## Summary

PR #6499 sped up `subtest { ... }` by dispatching the block through the VM's
compiled closure path (`vm_call_on_value` -> `call_compiled_closure`) instead of
the AST carrier (`call_sub_value` -> `eval_block_value` -> a fresh
`Compiler::compile()` on every invocation). The AST carrier's per-call recompile
is real waste, but the compiled-first dispatch regressed the vendored `Cro::HTTP`
suite and was reverted (`src/runtime/test_functions/tap_subtest.rs`,
`subtest_call_block` unconditionally calls `call_sub_value` again).

## Re-verification and corrected scope

Re-measured by re-applying #6499's exact dispatch decision behind a temporary
env-var gate on a debug build, and A/B-running the upstream test file
(`croservices/cro-http` at `6238e7539c32bd0e8b7962d38d778906af41f0a2`,
`t/http-middleware.rakutest`) with the full `-I` set from `batteries.lock`:

- AST carrier (current `main` behaviour): **24/24 subtests pass**.
- Compiled-first: **16 of 24 subtests fail.**

So the original write-up's "the byte-level `before-parse`/`after-serialize`
subtest silently became a no-op" badly understates it — that subtest (#5) is one
of sixteen. Failing: 3, 4, 5, 6, 7, 10, 11, 12, 13, 15, and the `throws-like`
inner subtests they contain. Passing: 1, 2, 8, 9, 14, 16, and the tail from 17 on.

## Hypotheses ruled out

The original file's working hypothesis was that this shares a root cause with the
registry snapshot/restore window. **It does not.** Three targeted A/B probes on
the same binary, all of which behave *identically* on both dispatch paths:

1. **Class declaration inside a subtest.** `my class K does R { has $.v = 7; ... }`
   declared in a subtest body: `.^name`, `~~ R`, `.new.go()`, the role-provided
   method, and the attribute default all resolve correctly on both paths.
   So it is not class registration, role composition, or attribute defaults.
2. **`LEAVE` phasers.** A `LEAVE` in the subtest body and a `LEAVE` in a nested
   bare block both fire, in the correct LIFO order, on both paths. This mattered
   because the failing Cro subtests use `LEAVE $service.stop()` and a
   non-firing `LEAVE` would leave `TEST_PORT` bound and cascade — it is not that.
3. **Async transform declared in a subtest.** A `my class` whose method returns
   `supply { whenever $in -> $m { emit $m.uc } }`, tapped and driven from the
   subtest body, delivers both values correctly on both paths.

Also note the failure is **not** a port/server cascade: in the compiled-first run
subtest 3's assertion 2 ("Got 200 normal response with an auth header") *passes*
while assertion 1 (a `throws-like` expecting the middleware's early 403) fails.
The server is reachable; the middleware's early-response half just does not run.

## Where to look next

The surviving pattern across the failures is the **early/conditional response**
half of middleware: `Cro::HTTP::Middleware::Conditional` (`emit` of either the
request or a `Cro::HTTP::Response`), `Cro::HTTP::Middleware::RequestResponse`'s
response part, `before-matched` block form, and byte-level `Cro::Transform`. The
request-only middleware in subtest 2 passes. That points at how the compiled
closure's frame interacts with Cro's router/pipeline when a value emitted from a
`whenever` short-circuits the downstream pipeline — not at declaration lifetime.

Suggested next step: instead of hunting from the Cro end, bisect from the
dispatch end. `call_compiled_closure` and `call_sub_value`'s compiled branch run
the same `CompiledCode` but build the frame differently — closure env merge order
and caller priority (`merge_all` / `is_authoritative` / the `self` force-install,
see the comments in `src/runtime/resolution_call_sub.rs`). Diff the two frames for
one Cro subtest under `rust-gdb -batch` breakpoints at both entries rather than
adding prints. ADR-0022/0023/0024/0025 are the prior art on this family
(a closure captured in one call context behaving correctly synchronously but
wrong when invoked later from another task).

## Reproduction

```
# temporarily restore #6499's dispatch decision in subtest_call_block:
#   if Sub carries compiled_code/compiled_routine -> self.vm_call_on_value(...)
#   else -> self.call_sub_value(...)
cargo build
git clone --depth 1 https://github.com/croservices/cro-http.git tmp/cro-http-tests
git -C tmp/cro-http-tests fetch --depth 1 origin 6238e7539c32bd0e8b7962d38d778906af41f0a2
git -C tmp/cro-http-tests checkout FETCH_HEAD
cd tmp/cro-http-tests && timeout 180 ../../target/debug/mutsu \
  -I ../../modules/Cro-HTTP/lib -I ../../modules/Cro-Core/lib -I ../../modules/Cro-TLS/lib \
  -I ../../modules/IO-Socket-Async-SSL/lib -I ../../modules/OO-Monitors/lib \
  -I ../../modules/IO-Path-ChildSecure/lib -I ../../modules/Base64/lib \
  -I ../../modules/HTTP-HPACK/lib -I ../../modules/Crypt-Random/lib \
  -I ../../modules/JSON-JWT/lib -I ../../modules/DateTime-Parse/lib \
  -I ../../modules/Log-Timeline/lib -I ../../modules/CBOR-Simple/lib \
  -I ../../modules/TinyFloats/lib -I ../../modules/OpenSSL/lib \
  -I ../../modules/MIME-Base64/lib -I ../../modules/Digest-HMAC/lib \
  -I ../../modules/Digest/lib t/http-middleware.rakutest
```

(The `-I` list is the `Cro::HTTP` row of `batteries.lock`; `scripts/battery-testsuite.sh`
builds the same command and is what caught the regression in CI.)

## Affected files

- `src/runtime/test_functions/tap_subtest.rs` (`subtest_call_block`)
- `src/runtime/resolution_call_sub.rs` (`call_sub_value`'s compiled branch,
  `is_authoritative` / `self` force-install)
- the `call_compiled_closure` / `vm_call_on_value` entry in `src/vm/`

## Relationship to ADR-0047

ADR-0047 removes the registry snapshot/restore window entirely, which decouples
declaration *lifetime* from the dispatch choice. That is a prerequisite for
re-landing #6499 (ADR-0047 phase P4) but is explicitly **not** a fix for the
regression described here — the probes above show the two are independent.
