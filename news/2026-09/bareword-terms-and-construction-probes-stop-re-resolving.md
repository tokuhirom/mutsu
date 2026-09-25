# Bareword type terms and construction probes stop re-resolving on every call

Part of ADR-0121 (#9291). `nqp::create(IB)` in a TRIR routine and a plain
`P.new(x => .., y => ..)` both did class-shape work on every call, even though
the answer depends only on the registry.

## What changed

- **Bareword terms in TRIR.** Every bareword term in a TRIR routine
  (`IB` in `nqp::create(IB)`, `nqp::istype($x, Map)`, ...) now compiles to
  `ClassOperand`, not just the class operand of a literal-name
  `getattr`/`bindattr`. `LoadBareWord` is gone from TRIR. The site remembers
  the type object it resolved to for one registry write generation. Because
  the value is used here, only an answer given by the name's own spelling is
  remembered, which is what the type-name branch of the resolution chain
  answers. A name that answers through a binding, an imported routine, or a
  nested class's qualified name is resolved again on every execution, as
  before.
- **`nqp::create`** looks up the class's short name through the per-symbol
  `unqualified_part` memo. It no longer builds a `StrSearcher` for
  `rsplit("::")` on every call.
- **Instance refcount shards.** The live-instance refcount shards now use
  `FxHashMap`. The ids are ours, so SipHash bought nothing, and it cost about
  280 instructions per instance, split between construction and drop.
- **`.new` call-site purity.** `mro_has_build_or_tweak` reads `has_build` /
  `has_tweak` from the cached `NativeCtorPlan`, instead of walking the MRO
  with two registry probes per level (about 4,100 instructions per `.new`).
  The plan's answer also counts a role's `TWEAK`/`BUILD`, which the old walk
  missed.
- **User-method probe.** The method-call preamble asks whether a user method
  must beat the native fast paths. That answer
  (`grammar_has_user_method_sym`) is now memoized per `(class, method)` for
  one registry write generation. Before, it cost an MRO walk plus a `String`
  allocation for the class name on every call.
- **Alias attributes.** The `has $x` alias attributes a native construction
  adds are collected into the plan once, instead of by an MRO walk on every
  construction.

## Measured

Release build, 4-core container:

| | before | after |
|---|---:|---:|
| `nqp::create(IB)` in a TRIR routine | ~650 ns | ~360 ns |
| `P.new(x => $i, y => 2)`, mainline loop | ~4,100 ns | ~3,350 ns |

Callgrind instructions per iteration (1 vs N iterations, differenced):

| | before | after |
|---|---:|---:|
| `nqp::create(IB)` | ~5,850 Ir | ~2,730 Ir |
| `P.new(...)` | ~33,640 Ir | ~26,960 Ir |

## What is left

The rest of `.new` is not attribute storage:

- the generic method-call preamble, about 13,500 Ir;
- the mainline loop's own `SetLocal` / `$i++` through the shared-var store.

Tests: `t/vm/nqp-bareword-type-operand-memo.t`, `t/oo/construct/construction-probe-memos.t`.
