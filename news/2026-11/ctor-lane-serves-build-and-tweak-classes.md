# The constructor lane serves classes with BUILD or TWEAK

`Class.new(named...)` compiles to `CallMethodMut`. That opcode walks a long
chain of speculative probes before it reaches the native default constructor.
The constructor lane (`src/vm/vm_ctor_lane.rs`, ADR-0121 D3) is a cache in
front of that chain. It records each class whose `.new` has been observed
reaching the constructor, so that later calls go there directly. Until now it
refused a class with a `BUILD` or `TWEAK` anywhere in its MRO, because those
submethods are user code and the lane marked its dispatch as pure.

Since #17919 the native constructor builds such classes in full: BUILD, the
deferred attribute initializers, TWEAK and the checks around them. So the lane
now admits them too. A replay runs the same `try_native_default_construct` the
chain's tail runs. For a class with a submethod, it leaves the dispatch impure,
as the tail does, and collapses a scoped overlay env first. The lane's other
guards are unchanged:

- the call shape;
- the registry generation;
- the identity of the `NativeCtorPlan`;
- user-declared classes only.

Callgrind on a profiling build. Each figure is the slope between two iteration
counts, second run of each binary:

| | before | after |
| --- | ---: | ---: |
| `class C4 { has $.id; has $!l = 5; submethod BUILD(:$id) { $!id = $id } }`, `C4.new(id => 1)`, Ir per construction | 44,700 | 36,800 |

The LLM::Chat `Response.new` + `emit` + `done` + `is-done` round from #17361
went from 1,397,000 to 1,161,000 Ir. That figure also includes #17919's
`attr_ref_scan` fix, which was not in the "before" binary.
