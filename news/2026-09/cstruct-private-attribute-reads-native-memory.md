# A private $!field on a CStruct handle reads native memory

`has int32 $!a` on an `is repr('CStruct')` handle read `Nil` instead of the
C struct's own bytes. The public accessor (`$obj.field`) compiles to a
method call and already went through `cstruct_field_value` (all five of
its call sites are method dispatch), but `$!field` — the private-twigil
form — compiles to a direct local-slot read (`GetLocal`) with no CStruct
fallback, so it always found the instance's empty Raku attribute map.

`exec_get_local_op_inner` (`src/vm/vm_var_assign_local_get.rs`) now tries
`cstruct_field_value` for a private-attribute local once the ordinary
self-cell lookup comes back empty — the same lookup the public accessor's
method-dispatch path already uses. This covers both a plain `has $!a` and
a `HAS`-declared embedded member read back through a method.

The write half (`$!a = 42` inside a method) stays a separate, wider touch:
`write_attr_cell_by_key` and its callers are `&self`, not `&mut self`, all
the way up through several files, so threading `&mut self` through for a
`cstruct_field_assign` fallback is left for a follow-up.

See [#8030](https://github.com/tokuhirom/mutsu/issues/8030) and the
regression test `t/nativecall/cstruct-private-attr-read.t`.
