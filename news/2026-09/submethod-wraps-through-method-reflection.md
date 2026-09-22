# Submethods can be wrapped through method reflection

`.^find_method` returns a `Submethod` object for construction hooks such as
`TWEAK`. mutsu already stored wrappers for ordinary `Method` objects in the
method-wrap registry, but rejected the corresponding `Submethod` object before
it could reach that registry. Modules such as `ValueClass` use this protocol to
wrap `TWEAK` during setup, so `use ValueClass` failed at load time.

Method reflection now accepts both `Method` and `Submethod` objects for
`.wrap` and `.unwrap`. `Functional::Queue` 0.0.2 consequently loads and its
test file passes under mutsu, matching Rakudo.

Pinned by `t/oo/method/submethod-wrap.t`.
