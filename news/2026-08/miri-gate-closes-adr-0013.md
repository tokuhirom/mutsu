# A Miri gate for the GC's aliased writes — and the one call site it proved was still UB

[ADR-0013](../../docs/adr/0013-container-interior-mutability-cellvalue.md) fixed the provenance UB
in the GC's aliased container writes by putting the payload in a `GcBox`'s `UnsafeCell`, so the
`&mut` that `gc_contents_mut` hands out has valid provenance even while other `Gc` handles read the
same node. What it shipped, though, was an **argument**. No ordinary test can see the difference: a
provenance violation is UB that happens to work, so `make test` and the whole roast suite stay green
either way. §4 phase 4 of the ADR therefore named a Miri run as the definition of done, and that
phase had never been built.

It is built now, as ci.yml's `miri` job.

## The gate

`cargo miri test --no-default-features --features native --lib gc::` on a pinned nightly. The
feature set drops the two things Miri fundamentally cannot execute — the Cranelift JIT, which emits
real machine code, and FFI. The nightly is pinned deliberately: Miri only ships on nightly, and an
unpinned one would silently change the interpreter (and its Stacked Borrows model) under the job,
turning a toolchain update into what looks like a soundness regression.

It is a **required status check**, not an informational one. On a repository where every PR
auto-merges, a `continue-on-error` job is a job nobody reads. Cost is controlled by the trigger
instead: `changes` now also classifies whether a change touches `src/gc/**` or `src/value/**`
(`scripts/ci-docs-only.sh --gc-value`, with its own self-test cases), and the job is gated on that —
23 of the last 300 merges, ~8%. The classification must be a job-level `if:` rather than a
workflow-level `paths` filter for the same reason the docs-only skip is: a check run that is never
created stays pending forever and blocks the merge it was supposed to guard. The `--gc-value`
classifier's fail-safe direction is the opposite of the docs-only one — an undeterminable diff runs
the check, because a skipped soundness check is a silently-unchecked merge while a needless one only
costs runner minutes.

Result on the current tree: **47 pass, 0 fail, 5 ignored**, about 19 seconds of interpretation
(wall clock is dominated by compiling the crate to MIR). The subset covers the collector, the `Gc`
primitive — including `gc_contents_mut_writes_through_a_shared_node`, the exact aliased-write shape
ADR-0013 made sound — and, since the filter is a substring match, `value::value_gc` as well.

## What writing it found

Rewriting `src/value/aliased_mut.rs`'s module header (it still described the provenance violation as
live and named Track B as the future fix, both false since ADR-0013) meant deleting the primitives it
documented as unused. The compiler disagreed: `arc_contents_mut` has a **live call site**.

`Mixin` is the one container variant that never migrated to the GC —
`ValueRepr::Mixin(Arc<Value>, Arc<HashMap<String, Value>>)` — and `$type.^set_name(...)` writes its
overrides map in place through `Arc::as_ptr as *mut`. An `Arc` payload has no `UnsafeCell`, so that
one site is still exactly the violation ADR-0013 removed everywhere else. ADR-0013 §8's claim that
the fix landed "at every call site at once" is wrong by one, and has been corrected in place;
`todo/tickets/mixin-overrides-aliased-write-is-still-arc.md` carries the repro and the two ways out.
The gate cannot see it yet — no test in the subset reaches `^set_name` — which is itself worth
knowing about a gate.

## What the gate does not cover, stated plainly

- **The VM's real call sites.** `src/gc/soundness_smoke.rs` adds four tiny Raku programs run through
  a real `Interpreter`, so Miri would watch the VM take an aliased `&mut` into a shared node while
  other handles are live — the coverage §4 warned a primitive-only run lacks. They are
  `#[cfg_attr(miri, ignore)]` today: `Interpreter::new()` eagerly builds `$*DISTRO`/`$*KERNEL` by
  shelling out to `uname -r` (twice, through two separate `OnceLock`s), `uname -m` and `hostname`,
  and Miri cannot spawn a process. `todo/tickets/magic-vars-should-be-built-lazily.md` fixes that at
  the root — those are process constants and should be delayed until first access and then cached,
  which also removes three `fork`/`exec`s from every `mutsu` startup.
- **Precision through the NaN box.** Miri reports `integer-to-pointer cast` on `Value` (ADR-0005) and
  falls back to permissive provenance for pointers recovered from the box, so checking is precise for
  typed `Gc<T>` paths and best-effort through the `Value` layer. `-Zmiri-strict-provenance` is
  unavailable to us for the same reason.
- **One race-hammer test.** `concurrent_buffer_and_drain_never_wraps_the_approx_count` (200k
  allocations across four threads against a spinning drainer) does not finish under Miri and is
  ignored there. It defends counter ordering, not provenance, and gc-stress still runs it natively.
  The workflow documents that escape hatch and the two conditions that make it legitimate, so the
  next person who hits a slow test does not instead weaken the gate.
