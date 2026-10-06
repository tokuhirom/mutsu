use Test;

# A role mixin over an object keeps the class's own `Numeric`, `succ` and
# `pred`: numeric operators, prefix `+` and `++`/`--` go through them, as they
# do for the bare object. (A typed `Pointer[T]` is exactly such a mixin, and
# NativeHelpers::Pointer's `$p - $q` and `$p++` rest on it.)
#
# Every expectation was verified against Rakudo.

plan 13;

class Num5 {
    has $.v;
    method Numeric { $!v }
    method succ { Num5.new(v => $!v + 1) }
    method pred { Num5.new(v => $!v - 1) }
}
role Tag { method hello { 'hi' } }

my $x = Num5.new(v => 5) but Tag;
my $y = Num5.new(v => 3) but Tag;

is $x - $y, 2, 'infix - numifies both mixins through Numeric';
is $x + 1, 6, 'infix + numifies the mixin';
is $x * $y, 15, 'infix * numifies both';
is +$x, 5, 'prefix + numifies through Numeric';
is -$x, -5, 'prefix - numifies through Numeric';
is $x.hello, 'hi', 'the role method is still there';

my $z = $x;
$z++;
isa-ok $z, Num5, '++ answers the class (via its succ)';
is +$z, 6, 'and the stepped value';
$z--;
is +$z, 5, '-- goes back through pred';

my $bare = Num5.new(v => 7);
is $bare - Num5.new(v => 2), 5, 'the bare object numifies the same way';

role Plain { }
my $i = 4 but Plain;
is $i + 1, 5, 'a mixin over a plain Int still numifies as that Int';
is +$i, 4, 'prefix + of it';
is ($i - (3 but Plain)), 1, 'two Int mixins subtract';
