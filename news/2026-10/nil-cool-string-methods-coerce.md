# Cool string methods on Nil coerce to the empty string

`Nil` is `Cool`, so `Nil.uc`, `Nil.chars`, `Nil.comb(/\S+/)`, `Nil.substr(0)` and the
other Cool string methods answer `""` and warn `Use of Nil.<method> coerced to empty
string`, as rakudo does, instead of returning `Nil` (#15226). This lets
Math::Libgsl::Raw::MovingWindow's `LIB` sub (`...comb(/\S+/).head`) yield `""` rather
than `Nil`, so `is native(LIB)` is accepted.
