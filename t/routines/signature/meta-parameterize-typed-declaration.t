use Test;

# A `C[T]` type spelled in a declaration, for a class with its own
# `^parameterize` (upstream NativeCall's `Pointer[uint16]`), is the type
# object that meta-method builds (#11203).

plan 4;

role BoxOf[::T] { method of { T } }
class Box {
    method ^parameterize(Mu \c, Mu \t) {
        my $w := c.^mixin(BoxOf[t]);
        $w.^set_name("Box[{t.^name}]");
        $w
    }
}

my Box[Int] $b .= new;
isa-ok $b, Box, 'my C[T] $x .= new builds an instance of C[T]';
is $b.of.^name, 'Int', 'which carries what ^parameterize mixed in';

my Box[Int] $u;
is $u.^name, 'Box[Int]', 'an uninitialized C[T] variable holds the C[T] type object';
nok $u.defined, 'which is undefined';
