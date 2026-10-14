# Regression test: `will leave` in a nested block of an EVAL string

`EVAL q[my $log = ""; { my $v will leave { $log ~= "left" }; $log ~= "in;" }; $log]` yields
`in;left`, as rakudo does. The case already works on `main` (the EVAL tail no longer travels
through the caller's topic, #13436); this pins it.
