# `nqp::push`, `nqp::unshift` and `nqp::bindkey` bind containers

The `nqp::` storage ops take their value operand as given: `nqp::push($l, $x)`
stores `$x`'s Scalar, so `$x = 6` afterwards shows in the slot, and
`nqp::iscont(nqp::atpos($l, 0))` is 1. mutsu already did this for
`nqp::bindpos` (#12786) but copied the value for `push`, `unshift` and
`bindkey`. The compiler now hands those three ops the variable's shared cell
too (#17658), the same lowering `bindpos` uses. A deconted operand
(`nqp::decont($x)`) or a literal stays a value.

`IterationBuffer.push($x)` (the method) still stores the value: a native
method row does not receive the caller's container yet (#17658).
