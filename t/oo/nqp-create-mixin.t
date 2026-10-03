use Test;
use nqp;

# `nqp::create` of a mixin type object keeps the mixed-in roles (#11209):
# upstream NativeCall's `CArray.new` is `nqp::create(self)` on the type its
# `^parameterize` built with `.^mixin`.

plan 9;

class C { has $.x }
role R[::T] {
    has $.r;
    method hi { "hi " ~ T.^name }
    submethod BUILD { $!r = 'built' }
}

my \M = C.^mixin(R[Int]);
M.^set_name('C[Int]');

my $o := nqp::create(M);
is $o.^name, 'C[Int]', 'the created object has the mixin type';
is $o.hi, 'hi Int', 'a role method is callable on it';
ok $o.defined, 'it is an instance, not a type object';
ok $o ~~ C, 'it is still a C';
ok $o.does(R), 'and does the role';
nok $o.r.defined, 'create runs no role BUILD';
is $o.x, Any, 'nor any class initializer';

# An object with a role mixed in creates an object with that role too.
my $p := nqp::create(C.new(x => 1) but R[Str]);
is $p.hi, 'hi Str', 'create of a mixed-in object keeps its roles';
nok $p.x.defined, 'and starts with unset attributes';
