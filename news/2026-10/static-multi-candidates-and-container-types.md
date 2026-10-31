# Multi candidates and parameterized container types for `static`

For `static` 0.0.1 (#15610), measured against rakudo 2026.09:

- the `compile-time-value` of a `multi sub` node is its own candidate of the
  dispatcher (it was the dispatcher), and a multi parameter's `meta-object` is
  its candidate's `Parameter`, so `where` constraints are found per candidate;
- `.candidates` hands out the same object each time (stable `.WHERE` /
  `nqp::objectid`), one id per declaration;
- `VarDeclaration::Simple.container-type` and `.return-type` keep the element
  type and parameterization (`Array[Int]`, `Hash[Str:D]`, `Array[Int]` as a
  parameter type).

`static` 0.0.1 `t/09-signature-coverage` goes from 26 to 0 `not ok` (rakudo: 0).

Also for `static`: a class, role, grammar, module or package declaration answers
`compile-time-value` (its type object, so `self` has a known type inside its
methods); a private method's value is the entry of the class's private method
table -- one stable object, its own sole candidate, no longer mixed up with a
public method of the same name; and `Array[T]` with a `::T` capture among its
arguments reports `^archetypes.generic`. `t/09-signature-coverage` is now 0
`not ok`.
