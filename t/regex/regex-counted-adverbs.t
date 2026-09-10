use Test;

plan 11;

my $data = "f fo foo fooo foooo fooooo foooooo";

ok($data ~~ m:nth(2)/fo+/, "m:nth(2) matches");
is($/, "foo", "m:nth(2) captures second match");

my $n = 3;
ok($data ~~ m:nth($n)/fo+/, "m:nth(\$n) matches");
is($/, "fooo", "m:nth(\$n) captures dynamic nth");

ok($data ~~ m:nth(2,3):global/(fo+)/, "m:nth(list):global matches");
is(@$/, <foo fooo>, "m:nth(list):global selects requested matches");

is("ABCDE" ~~ m:nth(*)/\w/, "E", "m:nth(*) picks the last match");

my $try = $data;
ok($try ~~ s:2nd{fo+}=q{bar}, 's:2nd{fo+}=q{bar} substitutes target occurrence');
is($try, "f fo bar fooo foooo fooooo foooooo", "s:2nd replacement wrote back");

my $try2 = $data;
nok($try2 ~~ s:7x{fo+}=q{bar}, 's:7x fails when not enough matches');
is($try2, $data, "s:7x leaves the original string unchanged");
