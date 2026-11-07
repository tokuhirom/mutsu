# A lazy `map` no longer counts named parameters as positional

`(0..*).map(&f)` over an infinite or lazy source sized the chunk it feeds the callback from
`Sub::params`, which also lists named parameters. `sub necklaces ($n, :ary($k) = 2)` was therefore
called with two elements and died with "Too many positionals passed". The chunk is now the number
of positional parameters (`make_lazy_pipe`), as rakudo does. Found through Math::Sequences'
`t/OEIS.rakutest` (A000031).
