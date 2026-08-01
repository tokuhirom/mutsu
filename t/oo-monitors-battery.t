use Test;

# OO::Monitors is provided NATIVELY (BATTERIES.md §7,
# docs/batteries/oo-monitors.md): `use OO::Monitors` enables the `monitor`
# declarator, and a monitor's instance-method calls serialize on a
# per-instance reentrant lock. The exhaustive check is the release-time gate
# running the full upstream suite (scripts/battery-testsuite.sh).

plan 6;

use OO::Monitors;

monitor Counter {
    has $!count = 0;
    method inc() { $!count++ }
    method bump-twice() { self.inc; self.inc }   # reentrant: no self-deadlock
    method current() { $!count }
}

my $cnt = Counter.new;
isa-ok $cnt, Counter, 'a monitor is a normal type';
is $cnt.current, 0, 'attribute defaults work';

$cnt.bump-twice;
is $cnt.current, 2, 'a monitor method can call sibling methods (reentrant lock)';

await do for ^4 {
    start { $cnt.inc for ^500 }
}
is $cnt.current, 2002, '4 threads of increments serialize to the exact total';

monitor Typed {
    has Int $.limit;
    method double() { $!limit * 2 }
}
my $t = Typed.new(limit => 21);
is $t.limit, 21, 'named construction works';
is $t.double, 42, 'methods see typed attributes';
