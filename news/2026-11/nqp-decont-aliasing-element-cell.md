# `nqp::decont` strips an aliasing element cell

`nqp::decont(%h.values[0]) =:= $obj` and `nqp::decont(.value) =:= SENTINEL`
over a hash's `.pairs` answered `False`: an element read through `.values` /
`.pairs` arrives as an aliasing cell, and `nqp::decont` only unwrapped a
variable reference, so the identity test compared the cell with the object.
`nqp::decont` now returns the object the cell holds. This brings Hash::Util's
`t/lock_keys.t` back: its `LockedHash` marks hidden keys with a sentinel and
finds them with exactly that test (#18812).
