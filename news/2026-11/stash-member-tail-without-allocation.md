# `.WHO` and package stashes build 61% cheaper

`stash_member_tail` decides whether a registered class, role or package name belongs to a
package's stash. A whole-stash build (`.WHO`, `P::`) calls it once per registered name, and each
call formatted two strings and built three substring searchers. It now matches on bytes without
allocating.

Measured with callgrind on a profiling build (slope between 100 and 300 iterations of
`$h = $x.WHO` on an Int, warm run): 1.16M -> 0.45M Ir per call. The stash build is still
O(registry size) (#9171); this only removes the per-entry constant.

Re-measuring ADR-15829 slice 1 on the same build showed unquoted `.HOW` at 3.9k Ir, `.WHAT` at
2.7k and `.DEFINITE` at 2.0k per call on an Int, so the slice's close condition holds.
