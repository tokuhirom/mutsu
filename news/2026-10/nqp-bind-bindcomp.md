# `nqp::bind` and `nqp::bindcomp`

Two more object-model `nqp::` ops are implemented (#11499).

`nqp::bind($var, $value)` compiles to `$var := $value`, the same way
`nqp::assign` already compiled to `=`. Binding one variable to another shares
the container: after `nqp::bind($x, $y)`, a later `$y = 7` is visible through
`$x`.

`nqp::bindcomp($lang, $compiler)` registers a compiler object for a
language, and `nqp::getcomp($lang)` returns it from then on. Binding `Raku`
replaces the built-in compiler object. A language that was never bound still
gives null. The registry lives in the REPL-compiler subsystem, alongside the
`Raku` compiler object that `getcomp` already served.
