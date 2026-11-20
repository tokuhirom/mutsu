# `$i++` on a `my int` local skips the generic increment checks

Every `++`/`--` of a scalar ran the same generic op. On each step it asked
whether the name was readonly, whether a shared cell or `Proxy` was parked in
the env under the name, whether the variable's type constraint made the store
something other than the identity, and whether a `state` slot or an env
mirror needed updating. For a `my int` that the routine declares itself and
nothing reads by name, the compiler already knows every one of those answers.

ADR-18357 (latch-guarded narrow ops, the first stage of ADR-18244 §O3) turns
that knowledge into a per-chunk proof bit, `CompiledCode::native_int_step_slot`.
The `++`/`--` arm tests the bit first. When the slot holds an `Int` and the
step stays in range, the op writes the new value and pushes the result
without any of the checks above. Anything else, such as a wrapping step, a
non-`Int` slot or a raised store spoiler, runs the generic op as before. The
JIT calls the same narrow arm through a dedicated shim instead of a full
interpreter dispatch. `MUTSU_NARROW_OPS=0` turns the narrow forms off, and
`t/vm/codegen/opcode-narrow-form-parity.t` checks that both settings print
the same output.

Sinking a payload-free word is now free as well. This covers a small `Int`, a
`Num`, a `Bool`, `Nil` and a type object, as in `$i++;` or `is-space($c);`.
Such a value has no `sink` of its own and is never a `Failure`, so the sink op
returns at once.

Callgrind slopes on a profiling build:
- `my int $p = 0; while $p < $n { $p++ }` went from 1,795 to 1,169
  instructions per iteration;
- a loop calling `is-space($c)` for its effect went from 11,180 to 9,357.

Most of what the loop still costs is entering the JIT twice per iteration,
once for the condition and once for the body (#18362).
