# A role multi method called on a mixin remembers its winner

`CArray[num64]` is the NativeCall `NumTypedCArray` role mixed into `CArray`, so every `@c[$i] = v`
and `@c[$i]` is a call to a multi method (`ASSIGN-POS` / `AT-POS`, 5-8 candidates) on a mixin
receiver, which re-matched every candidate's parameter types on each call (505k instructions per
store; Sort::Fast's 1M-element sort took 224 s). When every candidate is key-decided (built-in
type constraints, `::?CLASS` with a smiley as invocant, no `where` / subset / type capture /
role-parameter constraint), the winner is now remembered by (role declaration, candidate
identity, method, argument type keys, receiver definedness). `@c[$_] = 1.5e0` (callgrind slope,
profiling build): 505k -> 199k Ir per store (-60%); a three-candidate role multi call 341k -> 132k.
The remaining cost is per-call `RoleDef` / `MethodDef` clones and the `.WHAT` the `::?CLASS`
binding recomputes (#16215).
