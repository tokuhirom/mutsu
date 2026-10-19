use Test;

# A finite `.map`/`.grep` pipe over a `gather` is reified by a list coercion,
# whether the receiver is an expression or a variable: both VM method lanes
# read one forcing decision. The variable form used to skip the force and
# answer from the unpulled cache (`()`, `[(1 2)]`, `1`). A pipe that must stay
# lazy still does. Checked against Rakudo.

plan 12;

is-deeply gather { take 1; take 2 }.grep(* > 0).List, (1, 2), 'List of an expression';
my $p = gather { take 1; take 2 }.grep(* > 0);
is-deeply $p.List, (1, 2), 'List of a variable';
$p = gather { take 1; take 2 }.grep(* > 0);
is-deeply $p.Array, [1, 2], 'Array of a variable';
$p = gather { take 1; take 2 }.grep(* > 0);
is $p.list.elems, 2, 'list.elems of a variable';
my $m = gather { take 1; take 2 }.map(* + 1);
is $m.list.elems, 2, '... of a map';
$m = gather { take 1; take 2 }.map(* + 1);
is-deeply $m.values.List, (2, 3), 'values of a variable';
is-deeply gather { take 1; take 2 }.map(* + 1).values.List, (2, 3), 'values of an expression';
$m = gather { take 1; take 2 }.map(* + 1);
is-deeply $m.cache, (2, 3), 'cache of a variable';

my $inf = (1..*).map(* + 1);
is-deeply $inf.List.head(2), (2, 3), 'an infinite pipe stays lazy';
my $marked = lazy gather { take 1; take 2 }.map(* + 1);
ok $marked.List.is-lazy, 'a lazy-marked pipe stays lazy';
is (1, 2 ... *).values.head(2), (1, 2), 'values of an infinite sequence';
is-deeply gather { take 1; take 2 }.values.List, (1, 2), 'values of a gather';
