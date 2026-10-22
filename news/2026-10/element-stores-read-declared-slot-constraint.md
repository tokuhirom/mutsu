# Element, nested and push-family stores read a declared slot's constraint from the slot

`@a[i] = v`, `%h<k> = v`, the two-level and deep nested stores and the
`@a.push` family used to ask for the target's type constraint by name. A store
into a variable the running code declares (`BindingDesc::declared_here`) now
holds the compile-time slot (`target_slot` on `IndexAssignExprNamed`,
`IndexAssignExprNested`, `IndexAssignDeepNested`, `ArrayPush`) and reads that
slot's record in the running frame only, through `StoreDecl`
(`vm_slot_type_constraint.rs`). Only a name the code does not declare (a free
variable, an `EVAL`'d or symbolic name, a global, a sigilless alias target)
still looks up the by-name record. This is ADR-13757 step 3 for the
element-store handlers and part of #14632.
