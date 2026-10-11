# `.splice` on exceptions and encoding buffers raises X::Multi::NoMatch

`X::AdHoc.new.splice(1)`, a user `class E is Exception` instance and `"abc".encode.splice(1)`
raised `X::Method::NotFound`. The `Any.splice` row took them for `Mu`-rooted classes because it
read the registry MRO, which has no `Any` for a class with a builtin ancestor. `Mu`-rootedness is
now decided from the declared parents (`is_mu_rooted_class`), so these answer
`X::Multi::NoMatch` as in Rakudo (#12648, ADR-11276 §9.70).
