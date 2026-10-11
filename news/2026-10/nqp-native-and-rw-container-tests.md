# `nqp::iscont_i` / `_n` / `_s` and `nqp::isrwcont`

The last four missing Type / Conversion `nqp::` ops are implemented (#12673,
ADR-11553 §3.2), so that category of `docs/nqp-op-coverage.md` is now 53/53.

`nqp::iscont_i($x)` / `_n` / `_s` answer whether the operand is a *native*
container of that kind. That covers a `my int $i`, a native attribute, an element of a
`my int @a` or an `nqp::list_i`, and a reference from `nqp::atposref_i`, also
after it is bound to another name. `nqp::isrwcont($x)` answers whether the
binding is writable: a variable, an `is rw` / `is copy` parameter, an element or
an attribute is, but a read-only parameter over the caller's container, a
constant or a literal is not. The answers come from what the compiler knows about the
operand (its declaration, its `.VAR`, its name), because an `nqp::` op only
ever sees decontainerized values.

Along the way `nqp::iscont` learned to answer 1 for a `Proxy` and 0 for a
plain native parameter, `nqp::atposref_i` / `_n` / `_u` on a native-element
array return a real `IntPosRef` / `NumPosRef`, and `nqp::bindpos_*` into a native
integer array wraps to the element width as an assignment does
(`nqp::bindpos_u(@uint8, 2, 513)` stores 1). The `annotations` distribution's
`nqp::isrwcont($!value)` and the vendored NativeCall dispatcher's
`nqp::iscont_i` checks now get Rakudo's answers.
