# A leaf closure call no longer pays for a full closure frame

Calling a closure value (`$b($x)`, a `.map` or `.sort` callback) went through
`call_compiled_closure_in_unit` every time. That entry pushes a call frame and
a caller frame, installs `&?BLOCK`, the callable id, `@_` and the parameters
into a fresh env overlay, force-installs the captured bindings, snapshots the
free variables, and scans the overlay on return to write mutations back. For
`-> $a { $a + 1 }` none of that is observable, and the call cost about 12,900
instructions against about 4,200 for the same body as a named sub (#8335).

A pointy block whose bytecode is made only of pure value ops now runs through
a light closure call (`src/vm/vm_closure_light_call.rs`). The ops it admits read
their operands from the stack, a parameter slot or the closure's upvalue array:
constants, arithmetic, comparisons, string concatenation, jumps. A body with a
by-name read or write, a call, a declaration, a control exception or `$_`
keeps the full entry. The verdict is computed once per code object from its
bytecode (`src/light_closure_shape.rs`).

The call is checked too. Its arguments must bind exactly as the existing light
binds would bind them, and each bound value must be one `GetLocal` reads
straight from its slot. A `Nil` or a container argument, `.assuming`, a wrapped
closure and a captured return spec all decline to the full entry. The
`CallOnValue` op tries the light call before it decodes the call site's
argument sources, which a light body never reads.

Release callgrind, `MUTSU_JIT=off`, per call:

| call | before | after |
| --- | ---: | ---: |
| `-> $a { $a + 1 }` (whole loop iteration) | 12,950 | 2,630 |
| `sub named($a) { $a + 1 }` (whole loop iteration) | 4,230 | 4,230 |
| `-> $a { $a }` (whole loop iteration) | 12,710 | 2,400 |

`bench-ripemd-rounds` is 2.7% faster; `bench-fib`, `bench-multi-dispatch`,
`bench-array` and `poly-call` are unchanged.
