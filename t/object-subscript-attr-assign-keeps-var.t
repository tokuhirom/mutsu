use v6;
use Test;

# `$obj[i].attr = v` is lowered to "mutate the element, then write it back with
# `$obj[i] = element`". When the object declares no ASSIGN-POS/ASSIGN-KEY and is
# not a container subclass, that write-back fell through to the plain
# Array/Hash element-assign, which REPLACED the variable with a fresh container:
# `$obj` silently became an `Array` and every later method call on it died.
# The element is already mutated in place, so the write-back has nothing to do.

plan 8;

class Elem { has $.a is rw; has $.b is rw; }

class Box does Positional {
    has @!c handles <AT-POS elems>;
    submethod BUILD() {
        @!c := Array[Elem].new(:shape(2));
        @!c[0] = Elem.new;
        @!c[1] = Elem.new;
    }
    method tag() { 'box' }
}

my $b = Box.new;
$b[0].a = 11;
is $b[0].a, 11, 'indexed attribute assignment takes effect';
is $b.^name, 'Box', 'the object variable keeps its type';
is $b.tag, 'box', 'ordinary methods still dispatch on it';

$b[0].b = 22;
$b[1].a = 33;
is "$b[0].a() $b[0].b() $b[1].a()", '11 22 33', 'further indexed writes accumulate';
is $b.elems, 2, 'the delegated container is intact';

# Associative shape: same lowering through AT-KEY.
class Bag2 does Associative {
    has %!h handles <AT-KEY>;
    submethod BUILD() { %!h = (x => Elem.new) }
    method tag() { 'bag' }
}
my $h = Bag2.new;
$h<x>.a = 7;
is $h<x>.a, 7, 'keyed attribute assignment takes effect';
is $h.^name, 'Bag2', 'the object variable keeps its type (associative)';
is $h.tag, 'bag', 'ordinary methods still dispatch on it (associative)';
