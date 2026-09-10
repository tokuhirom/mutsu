use Test;

plan 5;

my $x = "hello";
ok "hello world" ~~ /$x/, 'scalar interpolation matches literal text';

my @a = "2", 23, rx/a.+/;
is ("b235" ~~ / b @a /).Str, "b23", 'array interpolation prefers the longest matching branch';

ok "a" ~~ /@("a", "b")/, '@(...) interpolates a list expression as alternation';
ok "aaaaab" ~~ /<{ rx/a+b/ }>/, '<{ ... }> interpolates expression results as regex';
nok "aaaaab" ~~ /"a+b"/, 'quoted literal text is not treated as regex interpolation';
