use v6;
use Test;

# ::?CLASS as a NON-invocant parameter type constraint.
#
# Pins the Test::Async::Hub shape (mzef Test-phase frontier):
#   method create-suite(::?CLASS:D: ::?CLASS:U \suiteType = self.WHAT, *%c)
# which used to die at parse time with
#   X::Redeclaration: Redeclaration of symbol '$self'
# because a `\`-sigilless param after the ::?CLASS pseudo-type fell into the
# bare-invocant branch and produced a second `self` param. Binding also
# type-checked against the literal string "::?CLASS:U" and always failed.

plan 8;

class Foo {
    method create-suite(::?CLASS:D: ::?CLASS:U \suiteType = self.WHAT, *%c) {
        suiteType.^name
    }
}

is Foo.new.create-suite(), 'Foo', 'invocant marker + ::?CLASS:U sigilless param with default parses and runs';
is Foo.new.create-suite(Foo), 'Foo', 'explicit type-object argument binds';

class A {
    method m(::?CLASS:U \t = self.WHAT) { t.^name }
    method sigiled(::?CLASS:U $t = self.WHAT) { $t.^name }
}
class B is A { }

is A.new.m(), 'A', '::?CLASS:U sigilless param defaults from self.WHAT';
is B.new.m(), 'B', 'inherited method: default resolves to the runtime class';
is B.new.m(B), 'B', 'subclass type object satisfies the declaring-class constraint';
is A.new.sigiled(), 'A', '::?CLASS:U on a $-sigiled param works too';

class C { method n(::?CLASS:D $other) { $other.defined } }
my $c = C.new;
ok $c.n(C.new), '::?CLASS:D param accepts an instance of the class';
dies-ok { C.new.n("nope") }, '::?CLASS:D param rejects a foreign value';
