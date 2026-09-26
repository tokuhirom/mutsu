# IO::Path instance methods on the type object die with X::Parameter::InvalidConcreteness

`IO::Path.e` (and the other `IO::Path:D:` methods: the file tests, `basename`,
`lines`, `words`, `comb`, `Numeric`, ...) used to die with `X::Method::NotFound`
or fall through to a generic `Cool` method when called on the `IO::Path` type
object. Since `Nil.IO` and `Str.IO` now return that type object, code like
`MoarVM::Bytecode`'s `self.setting($path).IO.e` reaches this call. It now raises
`X::Parameter::InvalidConcreteness` with Rakudo's message ("Invocant of method
'e' must be an object instance of type 'IO::Path', not a type object ..."),
through the same type-object concreteness check that already served
`X::NYI.throw` (#9570).

Writing the test exposed a regex bug, fixed in the same change: a `'` inside a
double-quoted regex literal was taken for the opener of a single-quoted one, so
a following `$var` was not interpolated (`/"method '" $m "' must"/` never
matched). The single- and double-quote scanners now share one routine that
tracks both quote families.
