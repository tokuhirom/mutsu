use Test;

# A method's exit merges its env writes back into the caller. A nested call
# flattens the scoped overlay, so the callee's overlay also carries every
# parent lexical/global it never touched; those are dropped from the merge when
# `cheaply_unchanged` proves them identical. These pin what the merge must still
# propagate, so the skip can never grow into a dropped write.

plan 10;

our $pkg-var = 'pkg-before';
my $outer = 'outer-before';
my @outer-list = 1, 2;
my %outer-map = a => 1;

class W {
    has $.n is rw;
    method touch-outer() { $outer = 'outer-after' }
    method touch-pkg() { $pkg-var = 'pkg-after' }
    method touch-dynamic() { $*dyn = 'dyn-after' }
    method push-list() { @outer-list.push(3) }
    method store-map() { %outer-map<b> = 2 }
    method read-only() { return $outer.chars }
    method sets-attr() { $!n = 42 }
}

my $w = W.new(n => 0);

is $w.read-only(), 12, 'a method that only reads a caller lexical returns the right value';
is $outer, 'outer-before', '...and leaves it alone';

$w.touch-outer();
is $outer, 'outer-after', 'a method writing a caller lexical still propagates';

$w.touch-pkg();
is $pkg-var, 'pkg-after', 'a method writing an `our` package variable still propagates';

{
    my $*dyn = 'dyn-before';
    $w.touch-dynamic();
    is $*dyn, 'dyn-after', 'a method writing a dynamic still propagates';
}

$w.push-list();
is @outer-list.join(','), '1,2,3', 'an in-place push on a caller array is visible';

$w.store-map();
is %outer-map<b>, 2, 'an in-place store into a caller hash is visible';

$w.sets-attr();
is $w.n, 42, 'an attribute write inside a method survives the merge';

# The construction path: TWEAK at two MRO levels, whose frames carry the whole
# global overlay and write only attributes.
class Base { has $.a; submethod TWEAK(:$!a) { } }
class Kid is Base { has $.b; submethod TWEAK(:$!b) { } }
my $k = Kid.new(a => 'x', b => 'y');
is $k.a ~ $k.b, 'xy', 'TWEAK at two MRO levels binds both attributes';
is $outer, 'outer-after', 'constructing through TWEAK does not disturb a caller lexical';
