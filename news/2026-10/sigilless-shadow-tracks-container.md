# A shadowed sigilless binding follows its container

When `my $t` is declared while a sigilless `t` (`my \t := $x`, `my \t = $y`) is
visible, bare `t` moves to the term namespace (#11994). The move copied the
value, so `my $x = 1; my \t := $x; { my $t = 5; $x = 3; say t }` printed 1
instead of 3 (#12441). The term key now names the sigilless binding's own slot,
so it keeps tracking the bound variable.
