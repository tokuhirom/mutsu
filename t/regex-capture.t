use Test;
plan 14;

ok "abc" ~~ /<a>/, 'named capture matches';
is $<a>, "a", 'capture variable set';

my $s = "abc";
$s.match(/<b>/);
is $<b>, "b", 'capture set via .match';

ok "hello" ~~ m/ (\w+) /, 'positional capture group matches';
"hello" ~~ m/ (\w+) /;
is $0, "hello", '$0 captures matched text';

ok "1" ~~ m/ (\d) <?{$0 < 5}> /, 'code assertion with $0 passes';
ok "9" !~~ m/ (\d) <?{$0 < 5}> /, 'code assertion with $0 fails for 9';
ok "9" ~~ m/ (\d) <!{$0 < 5}> /, 'negated code assertion passes for 9';
ok "1" !~~ m/ (\d) <!{$0 < 5}> /, 'negated code assertion fails for 1';

ok "x254" ~~ m/x (\d+): <?{$0 < 255}> /, 'multi-digit capture with ratchet';

my @a = 10, 20, 30;
is @a[*-1], 30, 'WhateverCode index on array';

my regex abc { abc }
"foo abc def" ~~ / <&abc> /;
nok $<&abc>, '$<&name> lookup parses and is not set for plain <&name> regex calls';

grammar T1 {
    token ws { 'x' }
}
ok 'x' ~~ /<T1::ws>/, 'qualified token lookup handles :: correctly';
is $<T1::ws>, 'x', 'qualified token lookup capture is set';
