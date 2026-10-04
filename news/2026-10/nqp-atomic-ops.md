# The atomic `nqp::` ops

The eleven ops of NQP's Atomic family are implemented: `atomicload`,
`atomicload_i`, `atomicstore`, `atomicstore_i`, `atomicinc_i`, `atomicdec_i`,
`atomicadd_i`, `cas`, `cas_i`, `atomicbindattr` and `barrierfull` (#11502).

They do not add a second atomic implementation. Each container-targeting op
compiles to the Raku-level form that gives the same answer: `atomicinc_i` to
`atomic-fetch-inc`, `atomicload` to `atomic-fetch`, `cas` to `cas`, and so on.
So a native lexical, a native attribute (`$!x` inside a method), an `is rw`
parameter and, for `cas`, an array element all reach the same locked cell
that `⚛++` and `cas` already use. A closure that captured the variable sees
the write, and four threads each doing 250 increments count to 1000.
`atomicbindattr` is `bindattr`, because an attribute store already goes
through the instance's locked attribute map. `barrierfull` issues a
sequentially consistent fence.

The `nqp::` coverage table now counts 460 of 577 ops. Atomic RMW on an array
element (`nqp::atomicinc_i(@a[0])`, and `@a[0]⚛++` at the Raku level) still
dies; that is tracked in #11812.
