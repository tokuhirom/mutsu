use Test;

# An undeclared return type names a different exception depending on how it was
# written: a `returns`/`of` TRAIT is X::InvalidType (with `.typename`), while a
# `-->` signature ARROW is X::Undeclared (with `what => "Type"`).

plan 7;

throws-like 'sub foo() returns Bar { }', X::InvalidType, typename => 'Bar',
    'returns <undeclared> is X::InvalidType';
throws-like 'sub foo() of Baz { }', X::InvalidType, typename => 'Baz',
    'of <undeclared> is X::InvalidType';
throws-like 'sub foo(--> Quux) { }', X::Undeclared,
    '--> <undeclared> is X::Undeclared';

# Declared return types in every form still work.
lives-ok { EVAL 'sub a() returns Int { 1 }' }, 'returns Int lives';
lives-ok { EVAL 'sub b(--> Str) { "x" }' }, '--> Str lives';
lives-ok { EVAL 'sub c() of Numeric { 1 }' }, 'of Numeric lives';

# A return type naming a same-unit declared class is fine.
lives-ok { EVAL 'my class MyType { }; sub d() returns MyType { MyType.new }' },
    'returns <locally declared class> lives';
