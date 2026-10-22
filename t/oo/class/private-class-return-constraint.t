use v6.d;
use Test;

plan 5;

# A routine whose return constraint names a module-private class is checked
# against that class, not evaluated as an expression.
module M {
    my class C {}
    our sub f(--> C:D) is export { C.new }
    our sub g(--> C:D) is export { 42 }
    our sub h(--> C) is export { C.new }
    our sub u(--> C:U) is export { C }
}

is M::f().^name, 'M::C', 'a definite return constraint on a private class passes';
is M::h().^name, 'M::C', 'a plain return constraint on a private class passes';
is M::u().^name, 'M::C', 'a :U return constraint passes the type object';
throws-like { M::g() }, X::TypeCheck::Return, 'the wrong type is rejected';
throws-like { M::f() ; M::g() }, X::TypeCheck::Return, 'a later wrong return is rejected too';
