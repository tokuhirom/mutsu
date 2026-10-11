# `.splice` on exception instances and encoding buffers raises X::Multi::NoMatch

`X::AdHoc.new.splice(1)`, `Exception.new.splice(1)` and `"abc".encode.splice(1)` raised
`X::Method::NotFound`; Rakudo raises `X::Multi::NoMatch` because `Any.splice` is a proto with no
candidates. The `Any.splice` row declined them because `is_user_class_without_any` read the
registered MRO of a class that inherits a builtin (`X::AdHoc -> Exception`), which stops at the
builtin without its `Any`, and so mistook it for a `class C is Mu`. A catalog ancestor now counts
as an `Any` root (#12648).
