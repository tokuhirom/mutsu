# A Callable subscript lets its exception through

`@a[-> $n { die "boom" }]` and `@empty[$i % *]` read as `Nil` in mutsu because the subscript
closure's error was swallowed (`call_subscript_code` returns `Nil` for a dying closure). The read
sites for Array, Hash, List/Seq and Range subscripts now use `call_subscript_code_checked`, so the
exception, or a returned Failure, surfaces as in rakudo (`Attempt to divide 3 by zero using %`).
Found by the raku-doc differential (`Language/subscripts.rakudoc:194`). The assignment, delete and
`Buf` offset sites still read a dying closure as `Nil`.
