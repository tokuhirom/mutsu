# A scalar bound to a type object stayed assignable

`$s := IB; $s = 1` (binding a scalar straight to a class or core type, then
assigning through it) silently succeeded and overwrote `$s` with `1`. rakudo
refuses this: binding to a bare type object leaves `$s` with no Scalar
container at all, the same shape as binding to an immutable literal (`$s :=
5`), so a later whole-value assignment dies.

`SetLocal`'s bind path (`src/vm/vm_var_assign_set_local.rs`) already refused
this for an allowlist of immutable scalar kinds (Int, Str, Range, ...), but
deliberately excluded `Package` (type objects), since an overlooked writable
kind there turns into a hard runtime error. Type objects needed their own
`ReadonlyKind::TypeObject`, not a fold into the existing `Immutable` kind,
because rakudo's wording for this shape names the type: "assign requires a
concrete object (got a IB type object instead)", not the generic "Cannot
assign to an immutable value".

Two pre-existing mechanisms had to learn about the new kind: the rebind path
that clears a stale readonly mark before installing a fresh one (#9277's
exception) now also spares a type-object mark the same rebind just made, and
a slot the compiler judged "authoritative" (skipping its env mirror as a
perf optimization) now also syncs when the store just marked the name
type-object-readonly, since the error's own wording has to read the bound
value back by name.

Verified the allowlist did not over-reach into shapes that stay genuinely
writable: an `is rw` accessor of an unset (type-object-holding) attribute,
and `return-rw` of a plainly-declared (never `:=`-bound) variable defaulting
to `Any`, both still write through.

Pinned by `t/vm/binding/bind-scalar-to-type-object-stays-readonly.t`. Closes
#9730.
