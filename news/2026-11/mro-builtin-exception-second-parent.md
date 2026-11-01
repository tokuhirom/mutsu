# `class C is A is Exception` linearizes `Exception` before `Any` and `Mu`

Built-in classes registered at start-up (`Exception`, `X::AdHoc`, ...) carry a
linearization that stops at the class itself, and `.^mro` appends `Any, Mu` for
display. Used as a later parent in a C3 merge, that missing tail let a sibling
parent's `Any, Mu` sort first, so `class Err is Event is Exception` printed
`(Err Event Any Mu Exception)`. The merge now completes a cached parent
linearization with `Any` and `Mu`, giving rakudo's `(Err Event Exception Any Mu)`.
Net::BGP's `Net::BGP::Error` has this shape.
