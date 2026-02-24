use Test;

plan 5;

class Base { has $.value is rw; }
class Exponent { has $.value is rw; }

multi sub infix:<+> (Base $b, Exponent $e) | (Exponent $e, Base $b) {
    $b.value ** $e.value
}

my $base = Base.new();
my $exp = Exponent.new();
$base.value = 2;
$exp.value = 5;

is($base + $exp, 32, 'first alternate signature dispatches');
is($exp + $base, 32, 'second alternate signature dispatches');

multi sub postfix:<!> (Base $x) | (Exponent $x) {
    state $counter = 0;
    ++$counter
}

is($base!, 1, 'first call sees shared state');
is($exp!, 2, 'second signature shares state');
is($base!, 3, 'state is shared across all alternates');
