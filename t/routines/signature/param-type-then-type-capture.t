use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): a
# parameter may carry a nominal type AND a `::T` type capture, in either order.
# The capture-first order (`::T Int:D $x`) already parsed; the type-first one
# (`Int:D ::T $x`) did not, because the branch that recognized the variable
# after a type read the capture's FIRST colon as the named-parameter marker and
# was left with `:T $x`. `Data::Record` writes both orders in one signature:
#
#     multi method new(::?CLASS:_ ::THIS: List:D ::T $original is raw, ...)
#
# `ParamDef` has carried the two halves separately since #7984
# (`type_constraint` + `type_capture`), so only the grammar was missing.

plan 12;

class C {
    method type-first(Int:D ::T $x) { T.^name }
    method capture-first(::T Int:D $x) { T.^name }
    method untyped(::T $x) { T.^name }
    method no-smiley(Int ::T $x) { T.^name }
    method pseudo(::?CLASS:_ ::THIS: $x) { THIS.^name }
    method both-orders(::?CLASS:_ ::THIS: Int:D ::T $x is raw) { THIS.^name ~ '/' ~ T.^name }
}

my $c = C.new;
is $c.type-first(1), 'Int', 'Int:D ::T $x captures the argument type';
is $c.capture-first(1), 'Int', '::T Int:D $x still captures it';
is $c.untyped(1), 'Int', 'a bare ::T is unchanged';
is $c.no-smiley(1), 'Int', 'Int ::T $x needs no type smiley';
is $c.pseudo(1), 'C', '::?CLASS:_ ::THIS: captures the invocant type';
is $c.both-orders(1), 'C/Int', 'both orders in one signature';

# The nominal half is still enforced -- the capture does not swallow it.
dies-ok { $c.type-first('a') }, 'Int:D ::T $x still type-checks the argument';
lives-ok { $c.capture-first(2) }, '... and accepts a matching one';

# The capture binds a real type object usable as a constraint.
class D {
    method same(Int:D ::T $a, T $b) { $a + $b }
}
is D.new.same(1, 2), 3, 'the captured type constrains a later parameter';
dies-ok { D.new.same(1, 'x') }, '... and rejects a mismatch';

# A `-->` return constraint after such a parameter belongs to the signature,
# not to the parameter (`Data::Record`'s own `--> ::?CLASS:D`).
class E {
    multi method make(::?CLASS:_ ::THIS: Int:D ::T $x is raw --> ::?CLASS:D) { self.bless }
}
isa-ok E.make(1), E, 'a --> return constraint after the capture parses';

# A named parameter written after a type is still a named parameter: only `::`
# opens a capture.
sub named(Int :$x = 5) { $x }
is named(:x(7)), 7, 'Int :$x is still a named parameter';
