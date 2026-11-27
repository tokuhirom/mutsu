# Planned instance method calls skip CallMethodMut's lane probes

A method call on a variable (`$o.m(...)`, the `CallMethodMut` opcode) whose
receiver is an ordinary object reached the plain-method lane only after a
dozen other lanes had refused it: `.?` misses, `Proxy` fetches,
pseudo-methods, `Attribute` metamethods, `take`, the `.^` lanes, scalar and
array receivers, the constructor lane. Each refusal is cheap. Together they
cost about 1,000 instructions on every planned call, a sixth of the call.

Such a call now goes to the accessor lane and then the plain-method lane at
the top of `exec_call_method_mut_named_op` (`vm_instance_method_front`). No
skipped lane can answer an instance receiver under a key the plain-method lane
holds. The front refuses the names and classes some of those lanes do match
(`new`, `take`, `WHAT`, `HOW`, `VAR`, `Attribute`, the BOOT classes). A
receiver that is not an instance is refused by one tag probe, and a miss skips
the later probes, so calls the front cannot serve cost what they did before.

A `MethodCallPlan` also records whether its method has a light named slurpy,
so a replay no longer scans the method's parameters twice per call to find
out.

One `$o.n0()` call drops from ~6,870 to ~5,850 instructions. On
`scripts/bench-det.sh`: `method-call` -4.8%, `poly-call` -4.4%, `bench-class`
-3.2%, `bench-multi-dispatch` -1.5%. Every other series stays within its
run-to-run noise. Part of #19232.
