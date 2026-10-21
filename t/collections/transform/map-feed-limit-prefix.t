use Test;

# A deferred .map read by prefix runs its callback only for the elements the
# reader needs, in one loop run, whatever shape the callback has (#14196).
plan 19;

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

my @r = 1..10;
my $rw = @r.map({ $_++ });
is-deeply $rw.head(3).list, (1, 2, 3), 'rw map: head(3)';
is @r.join(','), '2,3,4,4,5,6,7,8,9,10', 'rw map: only the pulled prefix is written back';

my @q = 1..8;
$k = 0;
my $rp = @q.map(-> $x is rw { $k++; $x += 100; $x });
is $rp[1], 102, 'rw param map: second element';
is $k, 2, 'rw param map: two callbacks';

$k = 0; my $k2 = 0;
my $c = (1..*).map({ $k++; $_ * 2 }).map(-> $x, $y { $k2++; $x + $y });
is $c[1], 14, 'chained map, two-parameter second stage: second element';
is "$k $k2", '4 2', 'chained map: callbacks interleave per group';

throws-like { (1..7).map(-> $x, $y { $x + $y }).eager }, Exception, message => /'Too few positionals passed; expected 2 arguments but got 1'/,
    'a short final group is a binding error';
is-deeply (1..6).map(-> $x, $y { $x + $y }).list, (3, 7, 11), 'whole groups map';
