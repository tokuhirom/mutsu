use Test;

plan 5;

my $str = 'abbb';
my regex rx { a b+ };

ok($str ~~ m:ex:x(2)/<rx>/, 'm:ex:x(2) finds two exhaustive captures');
is(~$/[0], 'ab', 'first exhaustive capture is shortest match');
is(~$/[1], 'abb', 'second exhaustive capture is next match');
ok($str ~~ m:ex:x(3)/<rx>/, 'm:ex:x(3) finds three exhaustive captures');
ok($str !~~ m:ex:x(4)/<rx>/, 'm:ex:x(4) fails when only three captures exist');
