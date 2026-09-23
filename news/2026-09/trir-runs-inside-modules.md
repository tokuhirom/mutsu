# TRIR finally runs inside a module, and JSON::Fast's decode is 40% faster

ADR-0110's typed, resolved IR accepted JSON::Fast's scanners (`nom-ws`,
`parse-thing`, `parse-obj`, `parse-array`, `parse-string`) at compile time, but
at run time it almost never finished a call. On
`benchmarks/bench-json-fast-spdx.raku` (100 records), `run_trir_from_outside`
bailed 1,299 times and completed 0 times, and the profiler's exact routine
entry counts were identical with `MUTSU_TRIR=on` and `off`. Each bail re-ran
the routine on the untyped path from its start, so the partial TRIR run was
pure overhead. Nothing reported it: a bail is silent by design, because the
answer is the same. Found by the #8673 investigation, fixed under #9072.

Three defects were stacked, and each one hid the next.

1. **A linked callee was looked up in the wrong table.** `CallTr` (and the
   bytecode `CallTrir` site) stored a `key` + `fingerprint` and looked the
   callee up at run time in the `CompiledFns` the frame had in hand. For a
   module routine that table is the routine's *own nested-sub* table, which is
   empty for `parse-array`. So every statically linked call inside a module
   missed. The link now holds the callee's chunk and package itself
   (`TrLink`), which is what "resolved at compile time" means. The table in
   hand is still consulted, but only to detect that a routine was replaced:
   a table that holds the key under a different body declines.
2. **An expression-position `return` escaped the linked frame.** JSON::Fast
   leaves its loops with `(return @result)` inside `nqp::while`. TRIR lowered
   only the statement form to its return op, and compiled this one as a call
   to a routine named `return`. The control exception it raised passed
   through every linked frame and was taken by the outermost untyped call
   site. With (1) fixed and (2) not, `from-json('[[1]]')` died with
   "additional content", because the outer call received the inner array and
   an unwritten `$pos`. Both forms now lower to the same op (`compile/ret.rs`).
3. **A linked callee's free `my` of a `module { }` block was not found.**
   `trir_outer_binding` consulted only the mainline capture buckets and the
   environment. A `my` inside `module JSON::Fast { ... }`, `$ws`, lives in
   the package-keyed stores the untyped read consults by the running
   routine's package. It now looks there too, before the environment, in the
   untyped path's order.

`MUTSU_VM_STATS` now reports the outcome of every entry, so this cannot go
quiet again:

```
[mutsu vm-stats] trir: entries=811 completed=811 bails=0 bind-declines=0
```

Measured on the 727-record document, release build, 4-core container: the
`from-json` call went from **3.2s to 1.9s** (3 runs each). The decoded result
is byte-identical to rakudo's (re-encoded with `:sorted-keys`). rakudo takes
0.065s on the same box, so this is still ~29x. It was never going to close
the gap on its own: the slow string path (`parse-string-slow` →
`unjsonify-string`, #9073) and the per-call cost remain. But the typed IR is
now actually the thing running JSON::Fast's scanners, which every later step
of #8673 needs.

While writing the regression test, a fourth bug turned up: mutsu drops the
extra pairs of `run(..., :env(%*ENV, |%extra))` (#9085). That made
`t/vm/codegen/adr0110-trir-differential.t` vacuous: its `MUTSU_TRIR => 'off'`
run silently ran with TRIR on. The test now builds its environment hash by
assignment, and does compare the two runs.

Pins: `t/modules/adr0110-trir-module-linkage.t` (a module shaped like the
decoder; it checks the answers, TRIR on = off, and the stats line showing
completion with no bail) and the now non-vacuous
`t/vm/codegen/adr0110-trir-differential.t`.
