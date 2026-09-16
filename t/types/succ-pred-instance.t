use Test;

plan 8;

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

# A user .succ/.pred that itself dies must propagate that exception rather
# than being silently swallowed and falling back to a plain numeric
# increment/decrement (found via Net::Netmask's `.next`/`.prev`, whose
# `succ`/`pred` recompute through a `dec2ip` sub that dies out of range).
class Bounded {
    has $.n;
    method succ { die "overflow" if $.n >= 10; Bounded.new(n => $.n + 1) }
    method pred { die "underflow" if $.n <= 0; Bounded.new(n => $.n - 1) }
}
my $g = Bounded.new(n => 10);
dies-ok { $g++ }, 'postfix ++ propagates a die from a user .succ';
my $h = Bounded.new(n => 0);
dies-ok { $h-- }, 'postfix -- propagates a die from a user .pred';
