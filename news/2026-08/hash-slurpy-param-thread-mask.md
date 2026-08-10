# `*%options` / `*@list` slurpy parameters no longer leak another thread's same-named binding

Scalar parameters are masked from the cross-thread shared-variable store
for the duration of a call (`mask_thread_redeclared_params`) so a nested
spawn cannot force-publish a shadowed value over an unrelated caller's
live entry of the same bare name. `@`/`%`-sigil parameter names were
never masked at all — a `*%options` slurpy hash (or any other `@`/`%`
parameter) bound in one thread stayed visible, and clobberable, through
the shared store by any other thread's live same-named variable.

This surfaced as a real Cro bug: `Cro::HTTP::Response.set-cookie($name,
$value, *%options)`'s `%options` resolved to an unrelated `%options`
live elsewhere in the Cro server/client machinery (listener
`host`/`nodelay`/`port` options) instead of the caller's flattened
`%cookie-opts`, so the `Set-Cookie` header went out with no `Path=/`,
breaking session-cookie matching on other routes
(`t/http-session-persistent.rakutest` subtest 16, "Using old session for
route 2", now passes).

## Fix

Extended `mask_thread_redeclared_params` to also mask **slurpy** `@`/`%`
parameters (`*@x`, `*%h`) — `&` stays unmasked (routines, not shared
mutable variables). A slurpy collects a fresh per-invocation value out of
thin air, never a caller's shared container, so nothing legitimate
depends on its bare name resolving to an outer binding.

**A plain (non-slurpy) `@`/`%` parameter is deliberately NOT masked.** A
first version of this fix masked every `@`/`%` parameter unconditionally
and broke `roast/S17-channel/stress.t`'s `bogosort_concurrent` test: an
ordinary `sub f(@list) {...}`'s `@list`, read back through the
shared-store fallback by a nested `start` block that did not capture it
lexically, needs that name lane exactly the way a plain `my @a`
declaration does for its `__mutsu_atomic_*` CAS copies. Only a slurpy's
name is safe to mask.

## Verification

- `t/http-session-persistent.rakutest` subtest 16 now passes (three other,
  separately-tracked failures remain in the same file — the closure-escape
  and non-match-value tickets already cover those).
- New pin `t/hash-slurpy-param-thread-mask.t`: a `*%options` slurpy and a
  `*@items` slurpy both resolve to their own call across 20 concurrent
  threads × 5 iterations each; a plain (non-slurpy) `@list` parameter
  stays visible to a nested spawn (the regression the narrowing above
  fixes), passing under both `mutsu` and `raku`.
- A minimal standalone repro for the original slurpy-hash clobber
  remained elusive (as the investigation ticket itself noted — several
  isolated attempts passed even on unfixed code); the concrete evidence
  is the Cro subtest and the roast regression this fix's narrowing
  avoided.
- The full `S17-*` concurrency roast sweep (98 whitelisted files, 0
  failures) and `make test` pass with no regressions.
