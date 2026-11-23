use Test;

# A method whose signature names a definite return value (`--> Nil`,
# `--> True`, `--> 42`) may not `return` an argument in its own scope; rakudo
# refuses it at compile time with an X::Comp::AdHoc, as it does for a sub
# (#18990). A bare `return`, and a `return` inside a nested block, are fine.
plan 8;

throws-like 'class A1 { method m(--> Nil) { return 5 } }', X::Comp, payload => /Nil/,
    'a class method with --> Nil';
throws-like 'class A2 { method m(--> True) { return 0 } }', X::Comp, payload => /True/,
    'a class method with --> True';
throws-like 'role R3 { method m(--> 42) { return 1 } }', X::Comp, payload => /42/,
    'a role method with --> 42';
throws-like 'class A4 { method !p(--> Nil) { return 1 } }', X::Comp, payload => /Nil/,
    'a private method';

eval-lives-ok 'class B1 { method m(--> Nil) { return } }; B1.new.m',
    'a bare return is allowed';
eval-lives-ok 'class B2 { method m(--> Nil) { if True { return 1 } } }',
    'a return inside a nested block is not checked';
eval-lives-ok 'class B3 { method m(--> Int) { return 1 } }',
    'a type return spec allows a return argument';
eval-lives-ok 'sub s(--> Nil) { return }; s()', 'a sub with a bare return';
