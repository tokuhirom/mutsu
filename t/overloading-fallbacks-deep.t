use Test;

plan 2;

class Base { has $.value is rw; }
class Exponent { has $.value is rw; }

multi sub infix:<+> (Base $b, Exponent $e) is deep { $b.value ** $e.value }

my $base = Base.new();
my $exp  = Exponent.new();
$base.value = 2;
$exp.value  = 5;

is($base + $exp, 32, 'overloaded infix:<+> is used');

$base += $exp;
is($base, 32, '+= falls back to overloaded infix:<+>');
