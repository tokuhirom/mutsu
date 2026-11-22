# An adverb on a type application is a compile-time error

`my $z = Array[Int]:D` compiled in mutsu and died at run time with "Cannot
resolve caller postcircumfix:<[ ]>(Array:U, Int:U, :D)": the parser took the
`:D` for a subscript adverb. A subscript on a type name is a type
application, not a subscript, and rakudo refuses the colonpair at compile
time with `X::Syntax::Adverb` ("You can't adverb Array[Int]"). mutsu now does
the same for any type name it knows, built-in or declared, including a
parametric role (`R[Int]:D`) and nested applications (#18323).
