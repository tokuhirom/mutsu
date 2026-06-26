use Test;

# An enum *value* name shares the type/term namespace, so declaring a class
# (or another type) of the same name afterwards is an X::Redeclaration.

plan 4;

throws-like 'enum Error ( Metadata => -20); class Metadata { }', X::Redeclaration,
    'class redeclaring an enum value is X::Redeclaration';
throws-like 'enum E <Foo Bar>; class Foo { }', X::Redeclaration,
    'class redeclaring a word-list enum value';
throws-like 'enum A (X => 1); enum B (X => 2)', X::Redeclaration,
    'two enums sharing a value name';

# An unrelated class after an enum is fine.
lives-ok { EVAL 'enum Color (Red => 1, Green => 2); class Shape { }' },
    'enum + unrelated class lives';
