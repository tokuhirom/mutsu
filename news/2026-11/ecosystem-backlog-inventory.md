# Ecosystem backlog inventory and design triage (2026-11-08)

All 44 open `ecosystem` issues were re-checked against `main` with a release build and
`checkout-dist.py` runs compared with raku. Two issues were closed as fixed (#17013, #16585), one
new soundness issue was filed (#17149: Monad `t/14` races the type-hook cache and then aborts with
heap corruption), fifteen issues got a `tier:*` label, and same-cause issues were linked as
sub-issues (#15762 <- #15371; #16939 <- #16948, #16949; #15967 <- #14684, #15077;
#15688 <- #14812, #15505).

Three costly-to-reverse decisions were written up as Proposed ADRs with a recommendation:
ADR-15967 (no start-order guarantee, as rakudo), ADR-15371 (the BEGIN-time `$*R`/`$*CU` surface
shared with the slang host) and ADR-16451 (do not copy rakudo's `.precomp` directory layout).
The full cluster table, dependency order and the next-ten list are in a comment on the
ecosystem dashboard (#15488).
