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

`static` 0.0.1 `t/09-signature-coverage` goes from 26 to 5 `not ok` (rakudo: 0).
