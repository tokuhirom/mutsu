unit role EqvHookRole;

# Never imports the hook's `&infix:<eqv>`, declares a local candidate of its own.
multi infix:<eqv>(Str $a, Int $b) { "LOCAL" }
method same($l, $r) { $l eqv $r }
method same-where($l, $r where { $l eqv $r }) { "where ok" }
method same-raku($l, $r) { $l eqv $r }
