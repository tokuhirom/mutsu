# `PROCESS::<$x> = Nil` now decays to Any, matching plain scalar assignment

`PROCESS::<$X> = Nil` (and its runtime-key sibling `PROCESS::{$k} = Nil`,
used by `//=`/`||=` compound assignment) stored a bare literal `Nil` instead
of decaying it to the `Any` type object, unlike an ordinary untyped scalar
assignment (`$x = Nil` leaves `$x === Any`).

```raku
PROCESS::<$X> = 42;
PROCESS::<$X> = Nil;
say PROCESS::<$X>.^name;   # raku: Any   mutsu (before): Nil
```

Root cause: `store_process_dynamic`'s scalar branch
(`src/vm/vm_var_assign_index_named.rs`) never ran the same `Nil`-to-`Any`
reset (`reset_nil_untyped_scalar`) that the `SetLocal` opcode already applies
to a plain lexical scalar assignment. Applying that same helper on this
write path fixes both the fixed-key (`PROCESS::<$x> = ...`) and
runtime-key (`PROCESS::{$k} = ...`) forms, since both route through
`store_process_dynamic`.

Pinned by `t/process-dynamic-nil-decay.t`.
