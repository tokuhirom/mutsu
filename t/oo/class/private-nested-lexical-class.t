use Test;

plan 3;

# A lexical class declared inside a method is not registered while the
# enclosing class registers; its `trusts` list and private methods are
# read from its declaration.
my class Outer2 {
    method m {
        my class Inner2 { trusts Outer2; method !q { "inner" } }
        Inner2.new!Inner2::q
    }
}
is Outer2.new.m, "inner", 'nested class trusting the enclosing class is callable';

throws-like 'class C { method !p { 1 }; method m { my class D { method n { self!p } } } }',
    X::Method::NotFound, 'nested class cannot call the outer private method via self!';

lives-ok { EVAL 'class E { method m { my class F { method !p { 1 }; method n { self!p } } } }' },
    'nested class calling its own private method compiles';
