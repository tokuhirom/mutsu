# `Cool.new` and other abstract built-ins build an instance

`Cool.new`, `Numeric.new`, `Real.new`, `Stringy.new` and `Callable.new` died with
`No such method 'new'`. Like `Any.new`, they now build a defined, attribute-less instance of
the type (`Mu.new`), so Data::Dump::Tree's `t/01_all.rakutest` structure, which holds a
`Cool.new()`, can be built.
