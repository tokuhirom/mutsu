use Test;
plan 14;

ok !(5 ~~ Iterable), 'Int is not Iterable';
ok !(5e0 ~~ Iterable), 'Num is not Iterable';
ok !("a" ~~ Positional), 'Str is not Positional';
ok !(1.5 ~~ Associative), 'Rat is not Associative';
ok !(2**70 ~~ Iterable), 'a big Int is not Iterable';
ok (1, 2) ~~ Iterable, 'List is Iterable';
ok [1] ~~ Positional, 'Array is Positional';
ok {a => 1} ~~ Associative, 'Hash is Associative';

sub takes-iterable(Iterable $x) { 'yes' }
my $five = 5;
is (try takes-iterable($five)) // 'no', 'no', 'Iterable parameter rejects an Int';
is takes-iterable((1, 2)), 'yes', 'Iterable parameter takes a List';

class Bag3 does Iterable { method iterator { (1, 2, 3).iterator } }
ok Bag3.new ~~ Iterable, 'a class composing Iterable still matches';
my $m = 5 but Iterable;
ok $m ~~ Iterable, 'a mixin of Iterable onto an Int matches';
ok !(5 ~~ Positional), 'Int is not Positional';
ok !("x" ~~ Iterable), 'Str is not Iterable';
