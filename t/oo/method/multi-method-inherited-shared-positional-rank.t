use Test;

plan 4;

class GenericGenerator {
    multi method generate(UInt:D $size = 1) { self.generate(:$size) }
    multi method generate(UInt:D :$size = 1) { 'generic' }
}
class DerivedGenerator is GenericGenerator {
    multi method generate(UInt:D :$size) { "B$size" }
}
is DerivedGenerator.new.generate(3), 'B3',
    'a derived named-only method wins over an unbound parent position';
is DerivedGenerator.new.generate, 'B1',
    'the parent forwards its defaulted positional argument to the child';

class SameOwner {
    multi method choose(UInt:D $size = 1) { 'positional' }
    multi method choose(UInt:D :$size = 1) { 'named' }
}
is SameOwner.new.choose(:size(3)), 'positional',
    'same-owner candidates keep their optional-position narrowness';

class NarrowParent {
    multi method choose(Int $value where * > 0) { 'parent' }
}
class WideChild is NarrowParent {
    multi method choose(Int $value) { 'child' }
}
is WideChild.new.choose(3), 'parent',
    'a constraint on a shared position still outranks the derived owner';
