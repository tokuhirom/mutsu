use Test;

plan 10;

# `(A || B) //= v`: the `||` yields the selected operand's container. When A is
# true the assignment is a no-op, otherwise it assigns into B (A is evaluated
# once).
my %h;
my $zero = 0;
($zero || %h<k>) //= 9;
is-deeply %h, {k => 9}, '(0 || %h<k>) //= 9 assigns into the hash element';
($zero || %h<k>) //= 1;
is-deeply %h, {k => 9}, '... and a defined element is left alone';

my $truthy = 5;
my $other;
($truthy || $other) //= 3;
is $other, Any, 'a true left operand selects itself: no assignment';

my @a;
($zero || @a[2]) ||= 4;
is-deeply @a, [Any, Any, 4], '||= through (0 || @a[2])';

my $calls = 0;
sub guard { $calls++; 0 }
my %m;
(guard() || %m<x>) //= 7;
is $calls, 1, 'the left operand is evaluated once';
is-deeply %m, {x => 7}, '... and the element is assigned';

my $undef;
my $fallback;
($undef // $fallback) //= 6;
is $fallback, 6, '(Any // $b) //= 6 assigns into $b';

# The shape that found it: a class caching a method per name.
class Cache {
    has %!meths;
    method can(Str \name) {
        callsame() || %!meths{name} //= do { (method { 42 }, ) }
    }
}
my $c = Cache.new;
is $c.can('answer').elems, 1, 'callsame() || %!meths{name} //= ... caches the entry';
is $c.can('answer')[0].(Any), 42, '... and returns it again';
is-deeply $c.can('nothing').elems, 1, 'every name is cached';
