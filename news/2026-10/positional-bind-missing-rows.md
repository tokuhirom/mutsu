Chained positional binding now refuses undefined intermediate rows instead of
creating arrays for them. For example, `my @a; @a[0][1] := 42` raises
`X::Multi::NoMatch` for `BIND-POS` and leaves `@a` empty, matching Rakudo.
Ordinary assignment still vivifies rows, and a type object with its own compiled
`BIND-POS` method can handle the bind itself.
