# Ecosystem sandbox sets NO_NETWORK_TESTING for both interpreters

The parity sweep runs each distribution's test suite inside `bwrap` with
networking fully disabled, but it did not set the ecosystem's conventional
`NO_NETWORK_TESTING=1` guard. A distribution that checks this variable (or
conditionally `require`s an optional module like `IO::Socket::SSL` to decide
whether to attempt a network test) had no way to tell the difference between
"the network module happens to be missing" and "the sandbox has no network at
all" — Rakudo's flat dependency closure genuinely lacks the optional module
and so skips, while mutsu bundles it as an intentional battery, reaches the
network call, and fails with a DNS error instead of skipping. That produced a
spurious regression against Rakudo purely from measurement-contract
mismatch, not from an interpreter bug.

`scripts/ecosystem_common.py`'s `sandbox_wrap` now passes
`--setenv NO_NETWORK_TESTING 1` into the `bwrap` invocation for both
interpreters. LWP::Simple 0.109, re-measured under the fix, moves from
`partial` (5/8 baseline files, 42/46 assertions) to `green` (16/16 files,
68/68 assertions) — the three previously-regressed network-guard files now
skip cleanly on both sides, matching the sandbox's already-network-less
confinement.

Tracked in mutsu issue #8844.
