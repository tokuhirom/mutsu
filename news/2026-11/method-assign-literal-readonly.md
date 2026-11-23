# `.=` on a literal raises X::Assignment::RO

`X.=meth` is `X = X.meth`, so its target has to be a container. mutsu ran the
method on a literal target and answered the result: `say "ab12".=uc` printed
`AB12`. rakudo dies with `X::Assignment::RO`, "Cannot modify an immutable Str
(ab12)" (#18986).

`.=` on a literal now computes the method and then fails the store with that
error, just as `"ab" = 1` does. Variable and element targets assign as before.

Still open on #18986: `my $x := "a"; $x .= uc` dies with rakudo's plain-`=`
message ("Cannot assign to an immutable value") rather than naming the value.
#19024 tracks a separate gap: `.=` on an `is rw` routine call
(`f() .= uc`) does not assign through the returned container.
