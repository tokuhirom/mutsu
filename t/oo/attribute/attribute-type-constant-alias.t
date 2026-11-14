use Test;
use NativeCall;

# An attribute typed by a constant alias of a type (`constant gp = Int;
# has gp $.d`) has the aliased type: rakudo settles the alias when the
# attribute compiles. mutsu kept the alias name, so `.type` was a package
# called `gp`, the type check accepted anything, and a CStruct member typed
# by an alias (Gnome::N's `constant \gpointer = Pointer`) defaulted to a
# fresh `Pointer` package rather than NativeCall's.

plan 7;

constant gp = Int;
constant \sl = Str;
constant \ptr = Pointer;

class A {
    has gp $.d;
    has sl:D $.s = 'x';
}

is A.^attributes[0].type.^name, 'Int', 'the attribute type is the aliased type';
ok A.^attributes[0].type === Int, 'the type object is the aliased type object';
is A.^attributes[1].type.^name, 'Str:D', 'a smiley on a sigilless alias is kept';
is A.new(d => 3).d, 3, 'a value of the aliased type is accepted';
throws-like { A.new(d => 'x') }, X::TypeCheck::Assignment,
    'a value of another type is rejected';

class S is repr('CStruct') { has ptr $.p }
ok S.new.p === Pointer, 'a CStruct member typed by an alias defaults to the type object';
is S.^attributes[0].type.^name, 'NativeCall::Types::Pointer',
    'the CStruct member type is the aliased type';
