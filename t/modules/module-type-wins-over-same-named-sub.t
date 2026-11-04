use Test;

plan 8;

# A declared module/class is a type: a same-named `sub` does not turn the bare
# term `foo` into a call, and `foo()` is the type's coercion type. The sub is
# reachable as `&foo()`.
{
    my module foo {}
    sub foo() { "OH HAI" }
    ok foo.HOW ~~ Metamodel::ModuleHOW, 'my module + sub: bare foo is the module type object';
    ok foo().HOW ~~ Metamodel::CoercionHOW, 'my module + sub: foo() is a coercion type';
    is foo().^name, 'foo(Any)', 'coercion type name';
    is &foo(), 'OH HAI', '&foo() calls the sub';
}

{
    module Mod1 {}
    sub Mod1 { "sub" }
    ok Mod1.HOW ~~ Metamodel::ModuleHOW, 'module + sub declared after: bare name is the module';
}

{
    class K2 {}
    sub K2() { 2 }
    ok K2().HOW ~~ Metamodel::CoercionHOW, 'class + sub: K2() is a coercion type';
    ok K2.HOW ~~ Metamodel::ClassHOW, 'class + sub: bare K2 is the class';
    is &K2(), 2, '&K2() calls the sub';
}
