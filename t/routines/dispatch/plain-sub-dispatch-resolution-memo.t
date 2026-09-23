use lib 't/lib';
use Test;
use PlainResolveMemoA;
use PlainResolveMemoB;

# #9081: the resolution of a bare name that is a plain (non-multi) routine does
# not depend on the arguments, so it is memoized per (name, compunit, package,
# lexical package) under the functions-map generation. These are the shapes
# where a memo keyed too coarsely would hand one caller another's routine. The
# subs take `Int:D` parameters and declare inner subs, which keeps them off the
# light call paths, so every call really goes through the resolver.

plan 6;

# 1. Two modules each keep a private `memo-helper`, and this file has its own.
#    Each must keep resolving to its own, whichever ran last.
sub memo-helper(Int:D $x, int $n) {
    my sub tag() { 'main' }
    tag() ~ ($x - $n)
}
my @got;
for ^2 {
    @got.push: run-a(10);
    @got.push: run-b(10);
    @got.push: (^3).map({ memo-helper(10, $_) }).join(',');
}
is @got.join(' '),
    ('A10,A11,A12 B0,B10,B20 main10,main9,main8' xx 2).join(' '),
    'compunit-private helpers of the same name stay apart';

# 2. The same bare name in two packages.
package P1 { our sub which(Int:D $x) { my sub t() { 'P1' }; t() ~ $x }; our sub go() { (^3).map({ which($_) }).join(',') } }
package P2 { our sub which(Int:D $x) { my sub t() { 'P2' }; t() ~ $x }; our sub go() { (^3).map({ which($_) }).join(',') } }
is (P1::go(), P2::go(), P1::go()).join(' '),
    'P10,P11,P12 P20,P21,P22 P10,P11,P12',
    'the package a bare name is resolved from is part of the memo';

# 3. An inner `my sub` shadowing an outer plain sub of the same name, called
#    on both sides of the excursion.
sub shadow(Int:D $x) { "outer$x" }
sub shadow-caller(Int:D $x) {
    my sub shadow(Int:D $y) { "inner$y" }
    shadow($x)
}
is (^3).map({ shadow($_) ~ "/" ~ shadow-caller($_) }).join(','),
    'outer0/inner0,outer1/inner1,outer2/inner2',
    'an inner sub and the outer one it shadows resolve per scope';

# 4. Wrapping a routine after it has been resolved is seen by later calls.
sub wrapped(Int:D $x) { my sub t() { 'w' }; t() ~ $x }
my @w = (^2).map({ wrapped($_) });
&wrapped.wrap(-> |c { 'wrapped-' ~ callsame });
@w.append: (^2).map({ wrapped($_) });
is @w.join(','), 'w0,w1,wrapped-w0,wrapped-w1',
    'a wrap after the memo was filled takes effect';

# 5. A binding failure is still a binding failure on every call: the memo
#    hands back the routine, not a verdict about the arguments.
sub strict(Int:D $x) { my sub t() { 1 }; $x + t() }
is strict(1), 2, 'a matching call runs';
throws-like { strict("x") }, X::TypeCheck::Binding::Parameter,
    'a non-matching call after a memoized one still fails to bind';
