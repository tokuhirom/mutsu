# v6.e `last VALUE` / `next VALUE`, and v6.d's compile-time rejection of `last(5)`

Under `use v6.e.PREVIEW`, `last VALUE` and `next VALUE` now end the loop (or the
iteration) and make `VALUE` that iteration's contribution to the loop's result,
as in rakudo: `do for ^5 { last $_ * 10 if $_ == 2; $_ }` is `[0 1 20]`. The
value travels on the loop-control signal (`OpCode::LastValue` /
`OpCode::NextValue`) and every collecting loop — `do for`, `.map`, and the
gather-lowered `do while` / `do loop` / `lazy for`, which `take` it — adds it to
its result. A `Label` value is still the labelled form (`last(FOO)`), and bare
words such as `last Nil` / `next Empty` are values.

Before v6.e the words accept no argument or a `Label` only. A literal argument
(`last(5)`, `next "a"`) is now rakudo's compile-time `X::TypeCheck::Argument`
("Calling last(Int) will never work with any of these multi signatures"), and a
non-`Label` value reaching the routine at run time is `X::Multi::NoMatch`
instead of `X::AdHoc` (#11073).
