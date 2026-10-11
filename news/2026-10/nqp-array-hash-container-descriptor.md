# `nqp::getattr` / `bindattr` of an Array or Hash `$!descriptor`

`nqp::getattr(@a, Array, '$!descriptor')` answered `Nil` (and a Hash answered
itself), and `nqp::bindattr` on it was a silent no-op. The descriptor is now a
`ContainerDescriptor` (`ContainerDescriptor::Untyped` for an unconstrained
container) that holds the container itself: `.of` and `.default` read the
container's element constraint and `is default` value at call time, so they
cannot drift from `@a.of`. `set_of` / `set_default` and `bindattr` of a
descriptor write those fields back; binding anything that is not a
`ContainerDescriptor` dies instead of being ignored (#10956, ADR-0121 D3).

`.name` is the stamped descriptor name, else the bare sigil: the variable a
container was declared under is not recoverable from the value alone. Unlike
rakudo, an element-type change made through the descriptor is enforced on
later assignments.
