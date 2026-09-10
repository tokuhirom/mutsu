use v6;
use Test;
use NativeCall;

# `constant Foo = Int` binds a type object, and Raku accepts the alias anywhere
# a type name goes. C bindings rely on it for platform types
# (`constant HANDLE = uint32; sub GetProcessHeap(--> HANDLE) is native(...)`).
# mutsu's compile-time signature validator rejected the alias outright, so the
# whole declaration failed with "Invalid typename" / "Type ... is not declared".

plan 6;

constant MyInt = Int;
constant HANDLE = uint32;

sub takes(MyInt $x --> MyInt) { $x + 1 }
is takes(41), 42, 'a constant type alias works as a parameter and return type';

sub native-ret(--> HANDLE) is native('nosuchlib') { * }
ok &native-ret.defined, 'a native return type spelled as a constant alias compiles';

sub native-arg(HANDLE $h --> uint32) is native('nosuchlib') { * }
ok &native-arg.defined, 'a native parameter type spelled as a constant alias compiles';

# The alias must not swallow the errors it sits next to.
throws-like 'sub bad(NoSuchTypeAtAll $x) { }', X::Parameter::InvalidType,
    'an undeclared parameter type is still rejected';
throws-like 'sub bad(--> NoSuchTypeAtAll) { }', X::Undeclared,
    'an undeclared return type is still rejected';
throws-like 'my package P {}; sub bad(P $x) { }', X::Parameter::BadType,
    'a package parameter type is still X::Parameter::BadType';

# vim: expandtab shiftwidth=4
