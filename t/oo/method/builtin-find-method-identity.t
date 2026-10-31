use Test;
use nqp;

# A method a built-in type inherits is one object: the analysis compares
# `Array.^find_method('pick')` with `List`'s to recognise the CORE method
# (#15610). Expected values are rakudo 2026.09's.

plan 5;

ok Array.^find_method('pick') === List.^find_method('pick'), 'Array inherits List\'s pick';
ok Array.^find_method('roll') === List.^find_method('roll'), '... and roll';
ok nqp::objectid(Array.^find_method('pick')) == nqp::objectid(List.^find_method('pick')), '... with one object id';
ok nqp::objectid(List.^find_method('pick')) == nqp::objectid(List.^find_method('pick')), 'a method keeps its object id';
nok Array.^find_method('push') === List.^find_method('push'), 'Array\'s own push is not List\'s';
