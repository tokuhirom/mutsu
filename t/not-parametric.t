use Test;

plan 10;

# Parameterizing a non-parametric type (a plain class / package / module) with
# `[T]` throws X::NotParametric. Only roles and built-in container types accept
# type parameters.

throws-like 'my package P { }; P[Int]', X::NotParametric,
    'package is not parameterizable';
throws-like 'my module M { }; M[Int]', X::NotParametric,
    'module is not parameterizable';
throws-like 'my class C { }; C[Int]', X::NotParametric,
    'plain class is not parameterizable';

# The `of T` form on a non-parametric parameter type is equally rejected.
throws-like 'my package P { }; sub foo(P of Int) { }', X::NotParametric,
    'package "of" parameter is not parameterizable';
throws-like 'my module M { }; sub foo(M of Int) { }', X::NotParametric,
    'module "of" parameter is not parameterizable';
throws-like 'my class C { }; sub foo(C of Int) { }', X::NotParametric,
    'class "of" parameter is not parameterizable';

# Built-in container types and roles ARE parameterizable.
{
    is Array[Int].^name, 'Array[Int]', 'Array[Int] parameterizes';
    is Hash[Str].^name.subst(/'['.*/, '').Str, 'Hash', 'Hash[Str] parameterizes';
}
{
    role R[::T] { method t { T } }
    is R[Int].^name, 'R[Int]', 'a parametric role parameterizes';
}

# A non-parameterized use of the class still works.
{
    my class D { method hi { 'hi' } }
    is D.new.hi, 'hi', 'the class itself is unaffected';
}
