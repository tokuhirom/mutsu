use Test;

# From Concurrent::Queue: `cas` on an rw attribute reached through its accessor.
plan 6;

class Node {
    has $.value;
    has Node $.next is rw = Node;
}

my $a = Node.new(:value(1));
my $b = Node.new(:value(2));
my $seen = cas($a.next, Node, $b);
ok $seen === Node, 'cas returns the previous value on success';
ok $a.next === $b, 'accessor attribute holds the new value';
my $seen2 = cas($a.next, Node, Node.new(:value(3)));
ok $seen2 === $b, 'failed cas returns the current value';
ok $a.next === $b, 'failed cas leaves the attribute alone';

# Contended CAS must not lose updates.
class Counter { has $.n is rw = 0; }
my $c = Counter.new;
await do for ^4 {
    start {
        for ^500 {
            loop {
                my $old = $c.n;
                last if cas($c.n, $old, $old + 1) == $old;
            }
        }
    }
}
is $c.n, 2000, 'contended accessor cas loses no increments';

class Self-Cas {
    has $.v is rw = 1;
    method swap { cas($!v, 1, 9); cas(self.v, 9, 10); $.v }
}
is Self-Cas.new.swap, 10, 'cas through self.accessor works inside a method';

done-testing;
