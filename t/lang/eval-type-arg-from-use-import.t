use v6;
use Test;

# A type argument (`Pointer[Pointer]`) that names a type the EVAL'd unit's own
# `use` imports is declared: the check for undeclared type arguments counts the
# types its `use` statements bring in, as the undeclared-name check does.

plan 4;

lives-ok { EVAL('use NativeCall; my class C { has Pointer[Pointer] $.p }; 1') },
    'an imported type as a type argument';
lives-ok { EVAL('use NativeCall; my Pointer[int8] $x; 1') },
    'an imported type as the base of a parameterized variable type';
throws-like { EVAL('my Array[Numerix] $x; 1') }, X::Undeclared::Symbols,
    'an undeclared type argument is still refused';
throws-like { EVAL('use NativeCall; my Array[Numerix] $x; 1') }, X::Undeclared::Symbols,
    'also next to a use of a module';
