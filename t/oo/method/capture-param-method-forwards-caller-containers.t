use Test;

# Intl::CLDR: a method taking `|c` and forwarding it to an `is rw` sub.
plan 2;

sub bump($x is rw) { $x++ }
class K {
    method foo(|c) { bump(|c) }
    method new(|c) { bump(|c) }
}
my $a = 1; K.foo($a);
is $a, 2, 'method foo(|c) forwards the container';
my $b = 1; K.new($b);
is $b, 2, 'method new(|c) forwards the container';
