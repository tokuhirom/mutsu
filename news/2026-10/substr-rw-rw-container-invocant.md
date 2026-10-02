# `.substr-rw(...) = v` writes through an `is rw` accessor or parameter

`class C { has $.s is rw = "abc" }; $c.s.substr-rw(0, 1) = "Z"` left `$c.s` as `abc`.
`sub f($x is rw) { $x.substr-rw(0, 1) = "M" }` left the caller's variable unchanged in the same way.
Rakudo gives `Zbc` and `Mbc`.

`assign_substr_rw` wrote the rewritten string back *by name*: into the variable the lowering
handed it, and nowhere at all for a method-call invocant. Rakudo declares the method with a raw
invocant (`method substr-rw(\SELF: ...) is rw`), so the write goes through the invocant's own
container. mutsu now does the same through ADR-0067's raw-invocant machinery. A new native row,
`native_method_writes_raw_invocant`, makes the VM's two gates box the invocant: the E6 accessor
producer and `box_raw_lvalue_invocant`. The method's native write then stores the result into that
container, with the container's type constraint enforced. The by-name path remains for the shapes
the VM does not box (#10790).
