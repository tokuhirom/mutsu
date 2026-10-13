use Test;

plan 4;

# #12907: inside `class Foo::IO::Str`, `Blob[uint8]` is the core
# parameterized Blob. The compound declared name `class Foo::IO::Blob`
# does not make `Foo::IO` a scope whose `Blob` shadows the core one,
# and a parameterization must not bypass that guard.
class Foo::IO::Blob does Blob[uint8] is repr('VMArray') { }
class Foo::IO::Str {
    has Blob[uint8] $!ords;
    has Blob[uint8] $.pub;
    method ords { $!ords //= "abc".encode("latin-1") }
    method take(Blob[uint8] $b) { $b.bytes }
    method name { Blob[uint8].^name }
}

is Foo::IO::Str.new.ords.list, (97, 98, 99), 'attribute typed Blob[uint8] accepts a core Blob[uint8]';
is Foo::IO::Str.new(pub => "xy".encode("latin-1")).pub.bytes, 2, 'public attribute too';
is Foo::IO::Str.take("abcd".encode("latin-1")), 4, 'parameter typed Blob[uint8]';

# A genuinely nested parametric role still resolves through its package.
module M {
    role R[::T] { method t { T.^name } }
    class C { has R[Int] $.x; method m(R[Str] $y) { $y.t } }
    our sub mk { C.new(x => R[Int].new).x.t ~ C.new.m(R[Str].new) }
}
is M::mk(), 'IntStr', 'a nested parametric role resolves inside its module';
