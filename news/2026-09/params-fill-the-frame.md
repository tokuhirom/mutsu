# When the parameters are the frame, they build it

ADR-0077's "strongest form" was described as needing the locals stack fused with
the operand stack, so that a callee whose locals are exactly its leading
parameters would pay nothing: the argument already sits in the cell its slot
wants. Most of that win turns out not to need the fusion at all.

The light call path was writing each parameter slot **twice**. Opening the frame
filled `num_locals` slots with `Nil` — an out-of-line `Vec::resize` /
`extend_with` call — and then the bind loop overwrote the parameter prefix with
the bound values. For a callee like `fib`, whose one local *is* its parameter,
every one of those `Nil`s was dead on arrival.

`CompiledFunction::params_fill_frame` is now true when the parameter slots are
`0..n` **and** nothing else is a local — `fib`, `tak`, every leaf accessor. It is
a per-callee property, settled where `param_local_slots` is precomputed rather
than re-derived per call. When it holds, `call_compiled_function_positional_light_at`
opens the frame **empty** and the bind loop pushes each bound parameter in order
through `Locals::put_param_slot`. Each slot is written once, and the fill call
disappears from the profile entirely — `Vec::resize` and `Vec::extend_with` are
simply absent now, in both the JIT-on and JIT-off profiles.

The seed loop that reads enclosing values into non-parameter slots is skipped in
that case too: by construction it has nothing to do, since every local is a
parameter and it skips those.

## Measured

callgrind, retired instructions, both binaries built from the same tree (the
change stashed for the baseline), outputs verified equal first:

| program | JIT on | | JIT off | |
| --- | ---: | ---: | ---: | ---: |
| `fib(22)` | 152 097 034 → 147 907 328 | **−2.75%** | 259 518 024 → 255 332 694 | **−1.61%** |
| `tak(14,7,0)` | 1 627 600 958 → 1 573 773 245 | **−3.31%** | 2 635 348 286 → 2 581 526 152 | **−2.04%** |
| a `while` loop (control) | 860 435 718 → 860 432 596 | −0.0004% | 1 364 731 677 → 1 364 731 057 | −0.00005% |

The control moves by 620 instructions out of 1.36 billion, which is what a
control should do.

An earlier reading of this change said the JIT-off configuration got *worse*.
That was a measurement error, and the mistake is the one #7579's method notes
name explicitly: the baseline number had been taken from a different commit's
binary rather than from the same tree with the change removed. Comparing
same-tree binaries turned a "+0.79% regression" into a −1.61% improvement. Never
compare an instruction count against a figure recorded from another commit.

## The trap in the push

The bind loop `continue`s when a parameter is omitted and no precomputed fill
exists for it. Under a `Nil`-filled frame that slot simply keeps its `Nil`; under
a push it would be *skipped*, and every later parameter would land one slot low —
silently, since the values are all well-formed. The push path pushes that `Nil`
explicitly, and `put_param_slot` carries a `debug_assert_eq!` comparing the slot
index against the frame's current length, so the `gc-stress` and `jit-stress`
jobs (which run the TAP suite on a debug binary) check the alignment across the
whole suite rather than only where a test happens to use a defaulted parameter.

## What is left of the "strongest form"

Only the argument *move* — the bind loop's `mem::replace` out of the operand
stack. That is the part that genuinely needs `locals_base = args_base`, and with
the fill already gone it is a smaller prize than it looked when the ADR was
written.

`vm_call_light_typed` and `vm_call_fast` still size their frames: their bind
loops have different shapes (alias seeding, a separate plan), and sharing an
abstraction across all three should be justified by a measurement rather than by
symmetry.
