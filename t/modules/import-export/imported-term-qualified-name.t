use Test;
use lib 't/lib';
use ImportedTermNamespace;

plan 6;

# GEOS::Native's GEOS constant made GEOS::Geometry.new stop at GEOS (#7988).
# An imported value term is complete only at a whole identifier boundary.
class ParserTermRoot::Thing {
    has $.value;
    method make($value) {
        ParserTermRoot::Thing.new: :$value;
    }
}

is ParserTermRoot, 'imported value', 'a standalone imported term still resolves';
is ParserTermRoot::Thing.^name, 'ParserTermRoot::Thing',
    'a qualified type is not split at the imported term';
is ParserTermRoot::Thing.new(value => 12).value, 12,
    'a qualified constructor accepts parenthesized arguments';
is ParserTermRoot::Thing.new.make(23).value, 23,
    'a method body can call a qualified constructor with colon arguments';
my $object = ParserTermRoot::Thing.new;
isa-ok $object, ParserTermRoot::Thing, 'a qualified type works as a call argument';
is (ParserTermRoot => 1).key, 'ParserTermRoot',
    'a fat-arrow key remains autoquoted';
