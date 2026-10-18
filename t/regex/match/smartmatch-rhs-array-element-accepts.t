use Test;

plan 5;

class VC { multi method ACCEPTS(VC:D: Any:D $topic --> Bool:D) { $topic > 3 } }
my @q = VC.new,;

is-deeply (5 ~~ @q.head), True, 'smartmatch against @a.head calls the element ACCEPTS';
is-deeply (2 ~~ @q.head), False, 'and honours its answer';
is-deeply (5 ~~ @q.tail), True, 'same for @a.tail';
is-deeply (5 ~~ @q[0]), True, 'same for @a[0]';
sub f(@r) { return 5 ~~ @r.head }
is-deeply f(@q), True, 'inside a sub taking the array';
