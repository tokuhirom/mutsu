# `.VAR.of` and `.VAR.default` of a bound name report the bound container

`my Int $x; my $y := $x; $y.VAR.of` answered `Mu` and `$y.VAR.default` `Any`:
the descriptor `.VAR` builds for a variable read only the name's own
declaration, and `$y` has none. A name bound to another container is that
container, so with no declaration of its own it now reports the bound cell's
`of` type and `is default` -- `Int` / `Int` above, and the same for a name
bound to a typed attribute (`my $z := $obj.x`), as in Rakudo (#18764).
