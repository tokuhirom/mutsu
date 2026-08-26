# A role-mixed class instance gists as `Foo+{Bar}.new`, not `Foo.new`

`say Foo.new but Bar` printed `Foo.new`, dropping the `+{Bar}` composition that `.^name` on the
same value already reported correctly. Raku prints `Foo+{Bar}.new(x => 1)` from both `.gist` and
`.raku`.

## Root cause

The ticket's hypothesis (that the gist path read the base type's name instead of routing through
`.^name`'s mixin-aware lookup) was right in outcome but not in location. The default object
representation is built by `Interpreter::default_instance_repr`
(`src/runtime/methods_instance_ops.rs`), which takes its `display_name` from the *invocant's own*
`class_name` — and it never sees the mixin, because `call_method_with_values_inner`'s
`ValueView::Mixin` arm ends with a blanket
`self.call_method_with_values(inner.as_ref().clone(), method, args)`. By the time the repr is
built, the composition has already been unwrapped away.

## Fix

That blanket delegation now special-cases the three representation methods (`gist`, `raku`,
`perl`, no arguments): it delegates as before, then retargets the *leading* type name from the
base type's own name to the mixin-aware `what_type_name` — the same name `.^name` reports. The
rewrite only fires when the delegate's output really did start with the base type's name followed
by `.new` or `<`, so a class with a custom `method gist`, or one that gists as its backing
array/hash (`is Array`/`is Hash` subclasses), is left exactly as it rendered.

Verified against raku for `say`, `.gist`, `.raku`, and `.^mixin(Bar)`, and pinned by
`t/role-mixin-survival.t`.
