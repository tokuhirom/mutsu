# A bound aggregate attribute answers `.VAR` as the aggregate

`$!val := Array[Str].new(...)` seats the aggregate itself in the attribute, so
`$!val.VAR.WHAT.^name` is `Array[Str]` and `nqp::iscont($!val)` is `0` in every
method, while an assignment still gives `Scalar` / `1`. Previously only the
method that did the bind saw the difference. AttrX::Mooish's typed-attribute
checks depend on it (#14434).
