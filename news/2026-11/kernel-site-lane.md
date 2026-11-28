# A kernel-planned call site runs the kernel straight off the operand stack

A method call whose cached plan carries a method kernel (`$p1.distance-to($p2)` in
`benchmarks/method-call.raku`) still went through the full plan replay before its kernel ran:

- decoding the call site's argument sources;
- moving the arguments into a fresh `Vec`;
- recording the sources as pending for a binder;
- re-checking the plan's per-call gates inside `run_method_call_plan`.

A kernel binds nothing and reads no argument source, so none of that was needed.

The site cache (`vm_method_site_ic`) now tries the kernel first, in `vm_method_site_ic_kernel.rs`. It checks the same gates the replay checks before its kernel: the receiver's alias metadata, the slot-frame entry conditions and the parameter types. It also requires every argument to be a plain instance of exactly the class its plan key names, and it reads the receiver and the arguments where they lie on the stack. A kernel bail changes nothing, and the call takes the ordinary replay. The kernel's attribute reads also stopped cloning the attribute's value before converting it to a machine number.

Callgrind slope per loop iteration on `benchmarks/method-call.raku`, profiling build:

| | before | after |
| --- | ---: | ---: |
| whole iteration | 23,574 Ir | 22,731 Ir (-3.6%) |
| the `distance-to` site (`try_method_site_ic`) | 4,016 Ir | 3,168 Ir (-21%) |
| allocations | 11.06 | 10.07 |
