use Test;

# Protocol::Postgres uses `with` to distinguish a parametric serializer type
# object from a concrete serializer value.
plan 1;

role ParametricDefinednessTest[::T] { }
is ParametricDefinednessTest[Int].defined, False,
    'a parameterized role type object is undefined';
