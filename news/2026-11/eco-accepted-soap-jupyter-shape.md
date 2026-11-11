# Ecosystem ledger: SOAP::Client accepted, Jupyter surrogate entries re-pinned

A review of the 2026-11-11 sweep's rakudo/MoarVM-specific failures found that
almost all of them were already in `ecosystem/exclude.txt` (the MoarVM::
namespace, `$*VM.name` / `$*RAKU.compiler.name` gates, rakudo internals).
Two loose ends remained in `ecosystem/accepted-divergences.toml`:

- **Jupyter::Kernel / Jupyter::Chatbook `t/05-autocomplete.rakutest`** still die on
  the surrogate codepoint that `(1 .. 0x1ffff).map({.chr})` produces (#14416), but
  one more assertion now passes before the die (`ok 10, nok 0` instead of
  `ok 9, nok 1`), so the exact-shape entries stopped matching and both files were
  graded `regression` again. The shapes are re-pinned to the current failure.
- **SOAP::Client `t/05-simple-service.rakutest`** skips on rakudo only because the
  oracle has no IO::Socket::SSL; mutsu bundles it, so the test runs its real flow
  and dies resolving `www.w3schools.com` inside the network-less sandbox. This is
  the Dev::ContainerizedService situation (#15525), recorded as an accepted
  divergence on the same decision.

REPL, the remaining candidate, needed no entry: re-measured on current `main` it
is green.
