# Arithmetic methods run as method kernels, without a frame

ADR-19315 listed method kernels as the second way to close the object-oriented rows without deoptimization. They are now in (`src/vm/vm_method_kernel.rs`).

A method qualifies when it is planned for the slot frame and its body is closed arithmetic over:

- its parameters;
- its receiver's scalar attributes;
- generated accessors of its parameters or of `self`;
- `+ - *`;
- `**` by a constant from 0 to 30;
- `.sqrt`.

`method distance-to(Point $other) { (($!x - $other.x) ** 2 + ($!y - $other.y) ** 2).sqrt }` is the shape. Such a call runs as a straight-line evaluation over machine numbers, with no frame, binder or dispatch around it.

The kernel is decided once per method-call plan by a closed op list, so a new opcode can only make kernels rarer. Every op it runs is pure:

- an attribute read through the body's attribute-site cache;
- an accessor read through the accessor lane;
- or arithmetic.

So a kernel that meets anything it cannot compute exactly *bails*, and the call runs on the slot frame as before from the same untouched state. That covers a value that is not a plain `Int`/`Num`, an `Int` overflow (which the generic path widens to a big integer), a cold attribute site, and an accessor the lane does not hold. This is ADR-19288's rerun-from-the-start rule, not a deoptimization.

On every entry the kernel requires no user infix operator, no wrapped routine and the kernels' global switch (JIT on, no profiler). The plan's registry generations pin everything else. After eight consecutive bails a kernel stops being tried.

Callgrind slope per loop iteration, profiling build:

| input | before | after |
| --- | ---: | ---: |
| `$sum += $p1.distance-to($p2)` (method-call's method) | 13.4k Ir | 6.6k Ir (-51%) |
