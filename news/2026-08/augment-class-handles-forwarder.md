# augment class methods now synthesize `handles` forwarders

`augment class Foo { method inner() handles 'uc' {...} }` previously did nothing with the
`handles` clause: `Foo.new.uc` dispatched straight to the built-in `Cool.uc` coercion instead of
forwarding to `inner`'s return value, because `augment_class`'s `MethodDecl` arm never synthesized
the delegation methods the ordinary class and role body walkers already build for `handles`.

Fixed by porting the same `Name`/`Rename` forwarder synthesis
(`registration_class.rs::make_delegation_method`) the class walker already uses, keyed off the
same `CompiledMethodDecl.handles` field the ADR-0019 D3 unification gave all three walkers.
Verified line-for-line against `raku` with `t/augment-method-handles-forwarder.t`.

`Wildcard`/`Regex` handle specs are wired the same way (pushed onto `class_def.wildcard_handles`),
but testing surfaced that a wildcard handle loses to a same-named built-in `Cool`/`Any` method
(e.g. `.uc`) on *any* class, not just an augmented one — a pre-existing, unrelated dispatch-order
bug now tracked at `todo/tickets/wildcard-handles-loses-to-builtin-cool-methods.md` rather than
folded into this fix.
