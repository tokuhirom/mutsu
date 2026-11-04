# A program's own `role REPL` is no longer redeclared by the core `REPL` class

mutsu registers rakudo's core `REPL` class (and the `Perl6::Compiler` object) the
first time the program names `REPL` or calls `nqp::getcomp`. A program that
declares its own `role REPL` -- the REPL distribution does, and its CodeUnit
dependency calls `nqp::getcomp("Raku")` -- hit "Redeclaration of symbol 'REPL'"
on the first use of the name. The core class is now left out when the program
already has a `REPL` class or role; the `Perl6::Compiler` object is still
registered. The REPL distribution's `t/01-basic.rakutest` passes like rakudo.
