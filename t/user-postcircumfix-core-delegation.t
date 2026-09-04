use v6;
use Test;

# The `Array::Rounded` idiom: a module captures the CORE subscript routine as a
# term, then adds its own candidates that delegate the ordinary shapes back
# through the captured term. The capture must denote the CORE routine -- when it
# resolved to the module's own (hoisted, not yet textually reached) candidates
# instead, the delegation recursed into itself and overflowed the stack.
# See docs/adr/0041-sub-hoisting-vs-compile-time-name-visibility.md.

plan 5;

class Rounded is Array {}

my constant &old-same = &postcircumfix:<[ ]>;

proto sub postcircumfix:<[ ]>($, |) {*}
multi sub postcircumfix:<[ ]>(Rounded:D \SELF, Int:D $index) {
    old-same SELF, $index
}
multi sub postcircumfix:<[ ]>(Rounded:D \SELF, Any:D \index) {
    old-same SELF, index.round
}

my Rounded $r = Rounded.new(10, 20, 30, 40);
is $r[1], 20, 'the Int candidate delegates to the core routine';
is $r[1.6], 30, 'the Any candidate rounds, then delegates';
is $r[2.2], 30, 'a fractional index rounds down through the same candidate';
is $r[3], 40, 'Int:D still beats Any:D for an integer index';
is old-same(Rounded.new(1, 2, 3), 2), 3, 'the captured term itself indexes natively';
