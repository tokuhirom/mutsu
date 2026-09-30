# Nested `++@a[i][j]` no longer eats the caller's pending operands

The prefix and postfix increment/decrement of a chained subscript (`++@w[0][1]`,
`@w[0][1]--`) emitted a `Pop` after each `SetGlobal` temp store. `SetGlobal` already consumes its
value, so the extra `Pop` removed one slot of the *caller's* operand stack whenever the enclosing
sub was itself evaluated inside an argument list (`r($direction, approximate(...))` received
`(Array, Nil)`). Dropping the four redundant pops fixes it.

Found by the `Time::Duration` ecosystem suite, whose `t/01_tdur.t` goes from a die at assertion 51
to 135/135. Pinned by `t/collections/subscript/incdec-nested-keeps-caller-stack.t`.
