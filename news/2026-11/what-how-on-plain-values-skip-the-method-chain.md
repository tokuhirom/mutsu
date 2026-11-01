# `.WHAT` and `.HOW` on a plain value skip the method-call chain

ADR-15829 (Accepted 2026-11-01), first slice. An unquoted, argument-less `.WHAT` / `.HOW` is a
macro in Rakudo (a user method of that name is reachable only through the quoted form), yet mutsu
sent it through `exec_call_method_mut_op` -> `call_method_with_values_inner` ->
`try_native_method` -> `dispatch_method_by_name_1` (about 25 functions) before reaching
`dispatch_what` / `dispatch_how`. A lane at the top of `exec_call_method_op_impl` and
`exec_call_method_mut_op_impl` (`vm_pseudo_method_lane.rs`) now asks those two directly when
the receiver is an `Int`, `Num`, `Str` or `Bool` value (or a type object) and the call is
unquoted, has no arguments, no modifier, no accessor-ref marker and no wrapped method. Every
other call, including `.VAR`, `.WHO`, `.WHERE`, `.DEFINITE` and any container, Proxy, Instance or
Failure receiver, takes the generic path unchanged. The ADR describes an opcode; the lane has the
same guard and miss tail without a new `OpCode` arm.

Measured with the profiling build against the main build taken just before (Ir per loop
iteration of `$s += $x.WHAT.defined.Int`, slope between 100 and 300 iterations, deterministic;
`.defined.Int` itself is ~6.8k of each figure):

| call | before | after |
| --- | ---: | ---: |
| `$x.WHAT.defined.Int`, `$x` an Int | 35,972 | 15,467 (-57%) |
| `$x.HOW.defined.Int`, `$x` an Int | 68,197 | 43,977 (-35%) |
| `$t.WHAT.defined.Int`, `$t` a type object | 19,636 | 19,725 (unchanged) |
| `T.WHAT.defined.Int` / `Int.WHAT.defined.Int` | 25,063 / 16,324 | 25,052 / 16,465 (unchanged) |

Type-object receivers already avoided the long chain, so they do not move. `.WHAT` on an Int is
now ~8.7k Ir (the ADR's target was <= 8k); `.HOW` is ~37k, almost all of it `dispatch_how`
itself. This does not change AttrX::Mooish (#15688): none of its 7 per-iteration pseudo-method
calls is in this set (table on #15829).
