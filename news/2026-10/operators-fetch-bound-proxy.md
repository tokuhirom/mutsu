# Operators FETCH a `:=`-bound Proxy variable

Prefix `-`, infix `x`, the four range constructors (`..`, `..^`, `^..`, `^..^`, so also prefix `^`)
and list subscripts now read a bound `Proxy` by value, as `+` already did: with
`my $p := Proxy.new(FETCH => method { 5 }, ...)`, `^$p`, `1..$p`, `"x" x $p`, `(1..6)[$p]` and
`-$p` answer what `raku` does (#12782).
