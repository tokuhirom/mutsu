# A method called on a captured scalar can assign through `SELF`

A closure whose only use of an outer scalar was a method call on it (`start { $y.hit }`)
captured the variable by value, so a method doing `nqp::assign(SELF, ...)` (or taking
an `is rw` invocant) wrote into the closure's copy. The receiver of a method call on a
captured plain scalar now counts as a possible write, like an `is rw` call argument
(#12042), and is boxed into a shared cell. This is the deterministic half of #16898
(Object::Delayed's trampoline resolved by a sibling thread); the remaining half is a
timing race with the main thread.
