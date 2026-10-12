# A routine's `$_` comes from its frame root, not a per-call insert

Every call path used to write a fresh `$_ = Any` into the new frame's env, and
most also wrote `$! = Nil`. Method calls, multi dispatch and constructors paid
one map allocation per call for it. A routine's frame root now owns `$_` the
way it already owned `$/` and `$!` (ADR-12529 phase 3). A by-name lookup
stops at the root and answers `Any` until the routine binds its own, so the
insert is gone from every path that gives the routine a root.

Along the way, a block created in a routine that never set `$_` captured the
program's topic instead of the routine's `Any`. That bug is fixed. Two
shortcuts that treat an empty env tier as invisible now leave a routine's frame
root in place. One of them had dropped the routine's identity, which a block's
`return` needs to find its target.

`method-call` runs 2.3% fewer instructions. `bench-ctor` and
`bench-multi-dispatch` each run 1.1% fewer, and the call-heavy rows make
10,000 fewer allocations.
