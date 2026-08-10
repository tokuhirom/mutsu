# Sibling `for`-loop iterations spawning `start {}` threads now keep their own per-iteration binding, regardless of value type

`for $client-a, $client-b -> $client { start { ... $client ... } }` — a
single- or multi-param `for` loop whose body spawns a thread per iteration —
previously could converge both spawned threads on the SAME loop-item value
whenever the loop item was not one of the closure-capture machinery's
"plain" scalar types (Int/Str/Num/... — an `Instance`, a `Channel`, etc. was
not). The two concurrently-live iterations' distinct `$client` bindings both
funneled through one bare-name cross-thread shared-store slot, so
last-writer-wins clobbered one thread's value with the other's. The bug
required a specific trigger (some earlier, unrelated, fully-completed
binding of the same bare name, e.g. a `given EXPR -> $client {...}` warm-up
elsewhere in the program) to manifest reliably; without it a race between
the two threads' first writes happened to land closer to correct in
practice, masking the underlying defect.

This was root-caused and fixed per
[ADR-0023](../../docs/adr/0023-binding-provenance-spawn-capture.md): a
**binding-provenance** axis was added to the spawn-time capture decision. A
new `Interpreter::active_loop_param_names` stack tracks which bare names are
currently bound as fresh, readonly, per-iteration `for`-loop parameters in
the executing frame chain (pushed/popped alongside the existing
`loop_local_vars` loop-body-declaration scope, and isolated across every
call-frame boundary — `with_nested_registers`, the fast/light/light-typed
call paths, and the general `push_call_frame`/`VmCallFrame` mechanism — the
same way `loop_local_vars` already is, so a callee's own free variable never
inherits an outer loop's parameter-name provenance).

`block_captured_scalars` now treats a name in that set as closure-owned
**regardless of its value's type**, so `clone_for_thread_for_block` skips
seeding it into the cross-thread bare-name lane entirely — the existing
spawn-time env clone (already correct per iteration) is left as the sole,
undisturbed source of truth for the spawned thread. No change to
`box_captured_lexicals`, no `ContainerRef`-wrapping of instances, and no
`SharedStore` structural change were needed.

An `<->`/`is rw` loop parameter (which writes back to the source element)
is excluded from the new tracking, keeping its pre-existing behavior
unchanged. Pinned by `t/for-loop-param-start-sibling-isolation.t`, covering
the warm-up trigger (via `given`, a plain block, and a renamed warm-up
variable), the no-warm-up case, a multi-param variant, and a Channel-typed
loop item exercised cross-thread — all verified against `raku` first.

Two adjacent, unrelated gaps were discovered and filed separately during
verification rather than folded into this fix:
`todo/tickets/do-for-expression-form-drops-multi-param-names.md` (the `do
for LIST -> $a, $b {...}` *expression* form never wires up
`ForLoopSpec::multi_param_names`, unlike the statement form) and
`todo/tickets/named-sub-reads-enclosing-for-loop-param-dynamically-not-lexically.md`
(a named `sub` called from inside a `for` loop body can read the loop's
current parameter value instead of its own lexical closure, when the names
collide).
