use Test;

# A `'` inside a double-quoted regex literal does not open a single-quoted
# literal, so a following `$var` is still interpolated.

plan 9;

my $m = 'e';
ok "a'e" ~~ /"a'" $m/, q{"a'" $m};
ok "a'e'b" ~~ /"a'" $m "'b"/, q{"a'" $m "'b"};
ok "method 'e' must" ~~ /"method '" $m "' must"/, 'apostrophes around an interpolation';
ok "a\"e" ~~ /'a"' $m/, q{'a"' $m};
nok "a'\$m" ~~ /^ "a'" $m $/, 'the variable is not matched literally';
ok "x\$m" ~~ /'x$m'/, 'a single-quoted literal still does not interpolate';

# In a double-quoted literal `@name` follows qq rules: bare is literal text,
# a zen slice interpolates the space-joined elements.
my @bar = <p q>;
ok "parameter '@bar'" ~~ /"parameter '@bar'"/, q{bare @bar in "..." is literal};
ok "x p q y" ~~ /"x @bar[] y"/, q{"@bar[]" interpolates the joined elements};
ok "a@b.com" ~~ /"a@b.com"/, q{"a@b.com" is literal};
