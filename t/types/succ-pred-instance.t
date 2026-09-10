use Test;

plan 6;

# User-defined .succ / .pred on a class should be invoked by ++ / --.
# This exercises the VM's unified compiled-first method dispatch for the
# increment/decrement smart paths (increment_value_smart/decrement_value_smart).
class Counter {
    has $.n;
    method succ { Counter.new(n => $.n + 1) }
    method pred { Counter.new(n => $.n - 1) }
}

my $c = Counter.new(n => 5);
$c++;
is $c.n, 6, 'postfix ++ calls user-defined .succ';
$c--;
$c--;
is $c.n, 4, 'postfix -- calls user-defined .pred twice';

my $d = Counter.new(n => 10);
++$d;
is $d.n, 11, 'prefix ++ calls user-defined .succ';
--$d;
is $d.n, 10, 'prefix -- calls user-defined .pred';

# Chained custom succ keeps returning fresh instances.
my $e = Counter.new(n => 0);
$e++ for ^3;
is $e.n, 3, 'repeated ++ accumulates via .succ';

# Mixed direction nets out correctly through succ/pred.
my $f = Counter.new(n => 100);
$f++; $f--; $f++;
is $f.n, 101, 'mixed ++/-- net result via succ/pred';
