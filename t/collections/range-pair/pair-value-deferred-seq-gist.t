use Test;

plan 5;

# A Pair (or Hash) whose value is a not-yet-iterated map Seq held in an
# item container must render that Seq's elements, not an empty list.
my $s = (1, 2).map({ $_ * 2 });
my $p = 1 => $s;
is $p.gist, '1 => (2 4)', 'Pair.gist reifies a deferred Seq value';

my $s2 = (1, 2).map({ $_ * 2 });
is (1 => $s2).Str, "1\t2 4", 'Pair.Str reifies a deferred Seq value';

is (do for 1..2 -> $v { $v => (1, 2).map(* * 2) }).gist, '(1 => (2 4) 2 => (2 4))',
    'Pairs built in a for loop body keep their Seq values';

my %c = a => [1, 2], b => 3;
is (hash do for %c.kv -> $k, $v { $k => $v ~~ Array ?? $v.map(* * 2).cache !! $v }).gist,
    '{a => (2 4), b => 3}', 'a Hash value that is a cached map Seq renders';

my $s3 = (1, 2).map({ $_ * 2 });
is ($s3,).gist, '((2 4))', 'a List holding an itemized deferred Seq renders';
