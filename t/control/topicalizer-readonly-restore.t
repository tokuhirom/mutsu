# Topicalizers save, mark and restore the topic's readonly state: a given
# over a value, a writable with nested in a readonly for, array for loops,
# smartmatch and s/// (#18274).
use Test;
dies-ok { given 42 { $_ = 1 } }, 'a given over a value has a readonly topic';
my $x = 5;
with $x { $_ = 6 }
is $x, 6, 'a with over a variable writes through';
for 1, 2 {
    is (try { $_ = 0; 'assigned' }) // 'died', 'died', "a for over values has a readonly topic ($_)";
    my $y = 1;
    with $y { $_ = 2 }
    is $y, 2, 'a writable with nested in a readonly for';
    is (try { $_ = 0; 'assigned' }) // 'died', 'died', 'and the for topic is readonly again after it';
}
my @a = 1, 2;
for @a { $_ *= 10 }
is-deeply @a, [10, 20], 'a for over an array has writable topics';
given 'abc' { my $m = $_ ~~ /b/; is ~$m, 'b', 'a smartmatch inside given' }
$_ = 'xyz';
s/y/Y/;
is $_, 'xYz', 's/// on the mainline topic';
my Int $t = 3;
given $t { is $_, 3, 'given over a typed scalar' }
done-testing;
