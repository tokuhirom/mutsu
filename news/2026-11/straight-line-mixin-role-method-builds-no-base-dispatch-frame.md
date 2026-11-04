# A straight-line role method on a mixin builds no base-dispatch frame

`dispatch_mixin_method_call` ranked the wrapped class's candidates of the called method
(`matched_deferral_candidates`, about ten interns and a candidate walk) and pushed a method
dispatch frame and a samewith context, so a `callsame`/`nextsame` in the role method could reach
the base object's method. A role method whose compiled body is a straight line of local reads and
`nqp::` ops (`CompiledCode::ignores_role_context`, e.g. `CArray`'s `AT-POS` /
`ASSIGN-POS`) contains no call at all, so it cannot defer: neither the expansion nor the frame is
built for it.

Callgrind slope on a profiling build, second run: fill 60.2k -> 59.8k Ir per store; fill +
`.Array` 147.1k -> 136.2k Ir per element (-7%). Refs #16215.
