use Test;

# `Class.new(named => args)` on a class with a user `new` asks whether any user
# candidate accepts the arguments before default-constructing. That answer is
# memoized per (class shape, named-argument names and value types); calls that
# alternate between a shape a candidate accepts and one it declines must keep
# getting the right constructor.

plan 14;

class P {
    has $.k;
    has $.v = 'default';
    multi method new(Int :$k!) { self.bless(:k($k * 2), :v('user')) }
}

for 1..3 {
    my $a = P.new(:k($_));
    is $a.k, $_ * 2, "an Int :k reaches the user candidate (round $_)";
    is $a.v, 'user', "and it built the object (round $_)";
    my $b = P.new(:k("s$_"), :v(7));
    is $b.k, "s$_", "a Str :k is declined by the user candidate (round $_)";
    is $b.v, 7, "and the default constructor took both attributes (round $_)";
}

my $int = 5;
my $str = 'five';
is P.new(:k($int)).v, 'user', 'a variable holding an Int';
is P.new(:k($str)).v, 'default', 'a variable holding a Str';
