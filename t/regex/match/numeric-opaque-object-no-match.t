use Test;

plan 5;

# Generic numeric infix candidates end in `.Numeric`. An object that provides
# no such method therefore fails candidate resolution; it must not silently
# reach mutsu's structural/float fallback.
class Opaque {
    has $.value;
}

throws-like { Opaque.new(value => 1) == Opaque.new(value => 2) },
    X::Multi::NoMatch,
    message => /'Numeric(Opaque:D: )'/,
    'numeric equality on opaque objects throws a numeric no-match';

sub add-in-another-routine($left, $right) {
    $left + $right
}

throws-like { add-in-another-routine(Opaque.new(value => 1), Opaque.new(value => 2)) },
    X::Multi::NoMatch,
    message => /'Numeric(Opaque:D: )'/,
    'arithmetic on opaque objects throws across a routine boundary';

throws-like { Opaque.new(value => 1) < Opaque.new(value => 2) },
    X::Multi::NoMatch,
    message => /'Numeric(Opaque:D: )'/,
    'numeric ordering on opaque objects also throws';

class Numifiable {
    has $.value;
    method Numeric { $!value }
}

is Numifiable.new(value => 2) + Numifiable.new(value => 3), 5,
    'a user Numeric method still supplies numeric operands';

is 'Broken' == 'Broken', True,
    'non-numeric strings remain lenient for bare-string enum compatibility';
