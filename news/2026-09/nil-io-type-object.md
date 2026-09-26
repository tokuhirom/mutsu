# `Nil.IO` and `Str.IO` are the `IO::Path` type object

`.IO` is a `Cool` method, so on an undefined invocant rakudo answers the
`IO::Path` type object. Before this fix, mutsu got it wrong in two ways:

- `Nil.IO` was swallowed by the Nil method-absorbing fallback and returned
  `Nil`.
- A Cool type object stringified to its gist, so `Str.IO` became a path named
  `"(Str)"`.

`IO` is now on the list of methods that `Nil` really answers
(`nil_absorbs_method` and the matching `MethodCall` arm). The `.IO` handler
returns `IO::Path` for a type object or `Nil`. A file test on the result dies,
as in rakudo. The error type still differs: mutsu raises `X::Method::NotFound`
where rakudo raises `X::Parameter::InvalidConcreteness`. That is tracked in
#9570.

Test: `t/io/nil-and-type-object-io.t`. Closes #9495.
