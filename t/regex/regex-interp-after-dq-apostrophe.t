use Test;

# A `'` inside a double-quoted regex literal does not open a single-quoted
# literal, so a following `$var` is still interpolated.

plan 6;

my $m = 'e';
ok "a'e" ~~ /"a'" $m/, q{"a'" $m};
ok "a'e'b" ~~ /"a'" $m "'b"/, q{"a'" $m "'b"};
ok "method 'e' must" ~~ /"method '" $m "' must"/, 'apostrophes around an interpolation';
ok "a\"e" ~~ /'a"' $m/, q{'a"' $m};
nok "a'\$m" ~~ /^ "a'" $m $/, 'the variable is not matched literally';
ok "x\$m" ~~ /'x$m'/, 'a single-quoted literal still does not interpolate';
