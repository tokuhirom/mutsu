use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# An imported enum with a key spelled `self` does not turn a method's `self` into
# the enum value: `self.new` in a class of the importing unit still sees its
# invocant (#17197). The key stays reachable by its qualified name.

plan 8;

use SelfKeyEnum;

class C {
    has $.x = 7;
    method make { self.new }
    method me { self }
    method name { self.^name }
}

is C.make.x, 7, 'self.new builds an instance of the class';
is C.new.me.x, 7, 'self is the invocant';
is C.new.name, 'C', 'self.^name is the class';
is C.new.me.WHAT.raku, 'C', 'self is not the enum type';

is Sector::self.raku, 'Sector::self', 'the key is reachable qualified';
is Sector::child.raku, 'Sector::child', 'the other keys are exported';
is child.raku, 'Sector::child', 'a key is a bare term in the importing unit';
is (Sector::self, Sector::child, Sector::parent).map(*.value).join(','), '0,1,2',
    'the enum keeps its values';
