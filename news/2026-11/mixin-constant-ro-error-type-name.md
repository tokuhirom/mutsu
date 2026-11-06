# `X::Assignment::RO` names the mixin type of a sigilless constant assigned in a closure

`my \v = 42 but role {}; sub msg(&c) { try c(); $!.message }; msg({ v = 1 })` said
`Cannot modify an immutable Int (42)`; rakudo says `Int+{<anon|1>}`. The error took the
type name from the binding cell the closure holds, not from the value inside it (#16861).
