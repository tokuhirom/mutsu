use v6;
use Test;

# Rakudo's Str.AST raises the compile errors that running the code raises:
# a type used before its declaration, and a term used before its declaration.
# A well-ordered unit still gives its tree.

plan 12;

for 'say A1; class A1 {}',
    '{ say B1; class B1 {} }',
    'sub f { C1.new }; class C1 {}' -> $code {
    dies-ok { $code.AST }, "type used before its declaration: $code";
}

for 'say x1; my \x1 = 1',
    'say x2; constant x2 = 1',
    'say x3; enum E1 <x3 y3>',
    '{ say x4; my \x4 = 1 }' -> $code {
    dies-ok { $code.AST }, "term used before its declaration: $code";
}

for 'class A2 {}; say A2',
    'my \x5 = 1; say x5',
    'constant x6 = 1; say x6',
    'say x7; sub x7 { 1 }',
    'role R3 {...}; say R3; role R3 {}' -> $code {
    lives-ok { $code.AST }, "well-ordered: $code";
}
