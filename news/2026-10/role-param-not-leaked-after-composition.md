# Role parameters no longer leak out of role composition

Composing a parameterized role with a non-empty body left its value parameter bound in the
composing scope, so the next composition restored that stale value and a closure escaping a role
method (`method m { -> $y { $param ~ $y } }`) read the first specialization's value. Parameters the
composing scope did not hold are now removed once the body has run. Found through the
`Hash::MutableKeys` distribution, whose `t/01-basic.rakutest` now passes 3/3.
