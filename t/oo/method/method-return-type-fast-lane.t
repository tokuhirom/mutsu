use Test;

# A method whose `--> T` is a plain core type is checked against the value
# without resolving the spec on every call. The lane must keep every outcome
# of the general check: a refused value, Nil, a Failure, `is rw` containers,
# explicit returns, a nested type that shadows the core name, and captures.

plan 17;

class C {
    method int(Int $x --> Int) { $x }
    method str(--> Str) { 42 }
    method nil(--> Int) { Nil }
    method fail(--> Int) { fail "boom" }
    method ret(--> Int) { return 7 }
    method bad-ret(--> Int) { return "x" }
    method named(Int $x, Bool :$flag = True --> Int) { $flag ?? $x !! -$x }
    has $.cell = 5;
    method rw(--> Int) is rw { $!cell }
}
my $c = C.new;

is $c.int(3), 3, 'a value of the declared type passes';
is $c.int(4), 4, 'the second call replays the lane';
throws-like { $c.str }, X::TypeCheck::Return, 'a value of another type is refused';
throws-like { $c.str }, X::TypeCheck::Return, 'and refused again';
ok $c.nil === Nil, 'Nil passes';
ok $c.fail ~~ Failure, 'a Failure passes unthrown';
is $c.ret, 7, 'an explicit return of the right type passes';
throws-like { $c.bad-ret }, X::TypeCheck::Return, 'an explicit return of the wrong type is refused';
is $c.named(2, :flag), 2, 'a named parameter next to a return type';
is $c.named(2, :!flag), -2, 'the named argument is honoured on every call';
$c.rw = 9;
is $c.cell, 9, 'an is rw method keeps its container';

# A nested class named like the core type changes what `--> Int` means.
class Outer {
    class Int { method gist { 'nested' } }
    method make(--> Int) { Int.new }
    method num(--> Int) { 5 }
}
is Outer.make.gist, 'nested', 'a nested Int is the return type of a method of its owner';
throws-like { Outer.num }, X::TypeCheck::Return, 'and the core Int is refused there';

# A type capture of the signature is what `--> T` names.
class Cap { method id(::T $x --> T) { $x } }
is Cap.id(5), 5, 'a type capture return type';
is Cap.id("s"), 's', 'the capture follows the argument';

# Before and after a capture was bound in the process.
is $c.int(8), 8, 'the plain lane still answers after a capture was seen';
throws-like { $c.str }, X::TypeCheck::Return, 'and still refuses';
