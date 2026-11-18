use Test;

# A built-in method called on an inline receiver (the plain `CallMethod`) is
# answered from its method-table row past the full path's probes. Every
# answer must be the full path's, including the ones that path decides by
# name or by receiver before it reaches the table.

plan 11;

my $s = 'ABCDEF';
is (^3).map({ $s.substr($_, 2).lc }).join(','), 'ab,bc,cd', 'a chained call on an inline receiver';
is (^3).map({ 'a b c'.words.join('-') }).join(','), 'a-b-c,a-b-c,a-b-c', 'chained list methods';

grammar G {
    token TOP { <a> <b> }
    token a { x }
    token b { y }
}
class A {
    method a($/) { make 1 }
    method b($/) { make 2 }
    method TOP($/) { make $<a>.ast + $<b>.made }
}
is (^3).map({ G.parse('xy', :actions(A)).ast }).join(','), '3,3,3', '.ast and .made of an inline Match';

# `.raku` of an itemized list keeps its `$`.
is (1, 2).item.raku, '$(1, 2)', '.raku of an inline itemized list';

# A native array's answer keeps the receiver's native type.
my int @n = 3, 1, 2;
is (@n).sort.^name, 'array[int]', '.sort of an inline native array is retyped';
is (@n).reverse.^name, 'array[int]', '.reverse of an inline native array is retyped';

# An augment of a built-in type is seen by an inline receiver too.
use MONKEY-TYPING;
augment class Str { method shout() { self.uc ~ '!' } }
is ('a' ~ 'b').shout, 'AB!', 'an augmented method on an inline Str';

# A Failure argument still explodes under `use fatal` (the receiver has a shape).
{
    use fatal;
    sub f { fail 'nope' }
    dies-ok { 'abc'.index(f()) }, 'a Failure argument explodes under use fatal';
}

# A Junction argument autothreads.
is ('abc').index(any('a', 'c')).raku, 'any(0, 2)', 'a Junction argument autothreads';

# A method with no row on a shaped inline receiver still works.
is ('a,b').split(',').elems, 2, 'a method chain mixing rows and cascades';
is (^3).map({ (1/3).numerator }).join(','), '1,1,1', 'a Rat row on an inline receiver';
