use Test;

plan 5;

ok ((WHAT any()) ~~ Junction), 'WHAT any() parses and evaluates as a Junction type object';
ok ((WHAT any(1,2)) ~~ Junction), 'WHAT any(1,2) parses and evaluates as a Junction type object';

my $roll = 3;
ok($roll == any(1..6), 'any(1..6) expands finite ranges');

my @subs = (sub { 3 }, sub { 2 });
is any(@subs)().raku, (3|2).raku, 'calling any(@subs)() threads callable junction';

is (any("4","5") ~~ /4/).gist, 'any(｢4｣, Nil)', 'regex smartmatch on any junction keeps Nil non-matches';
