use Test;

# A deferred .map read by prefix runs its callback only for the elements the
# reader needs, in one loop run, whatever shape the callback has (#14196).
plan 11;

my $n = 0;
my $s = (1..*).map(-> Int $x { $n++; $x });
is $s[2], 3, 'typed-parameter map: third element';
is $n, 3, 'typed-parameter map: three callbacks';

my $k = 0;
my $t = (1..10).map({ $k++; $_ * 2 });
is-deeply $t.head(3).list, (2, 4, 6), 'head(3) of a map';
is $k, 3, 'head(3) runs three callbacks';

$k = 0;
my $u = (1..10).map(-> $a, $b { $k++; $a + $b });
is $u[1], 7, 'two-parameter map: second element';
is $k, 2, 'two-parameter map: two callbacks';

$k = 0;
my $v = (1..*).map({ $k++; slip($_, $_) });
is-deeply $v.head(3).list, (1, 1, 2), 'slipping map: head(3)';
is $k, 2, 'slipping map: the overshooting slip is not followed by another call';

$k = 0;
my $w = (1..*).map({ $k++; $_ %% 2 ?? Empty !! $_ });
is-deeply $w.head(3).list, (1, 3, 5), 'map that skips elements: head(3)';
is $k, 5, 'map that skips elements: five callbacks';

my @a = 1..5;
my $m = @a.map({ $_ + 1 });
@a[3] = 100;
is-deeply $m.head(5).list, (2, 3, 4, 101, 6), 'a Live source is read when the loop reaches it';
