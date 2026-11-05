# `$a.AT-POS(0) = v` on a plain scalar assigns the variable

A non-positional scalar is its own 0th element, and `.AT-POS(0)` answers its container
(rakudo/rakudo@cfb221c3e2, #15933), so `my $a = 42; $a.AT-POS(0) = 666` leaves `$a` at
666. mutsu raised `X::Assignment::RO: cannot assign through .AT-POS on non-instance`.
The lvalue-method assignment now stores into the invocant variable for an
Int/Num/Str/Bool/Rat/Complex value and index 0; Array and Hash targets are unchanged.
`roast/S09-multidim/XX-POS-on-undimensioned.t` passes (54/54) and is back in the whitelist.
Pinned by `t/collections/subscript/at-pos-assign-plain-scalar.t`.
