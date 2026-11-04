# A routine without a declared return type is not a `Callable[T]`

`my Callable &c = sub { 2 }` and `my Callable[Int] $y = sub { 1 }` now die with
`X::TypeCheck::Assignment` as in rakudo: only a routine that declares a return
type composes `Callable[T]`, and an explicit `Callable` type on a `&` variable
parametrizes it like any other (`Callable[Callable]`) (#16461).
