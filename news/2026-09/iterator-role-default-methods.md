# A class that `does Iterator` gets the role's default methods

A user class that composes `Iterator` and writes only `pull-one` now gets the
role's default methods, as it does in rakudo. These are `push-exactly`,
`push-at-least`, `push-all`, `push-until-lazy`, `sink-all`, `skip-one`,
`skip-at-least`, `skip-at-least-pull-one`, `is-lazy`, `is-deterministic` and
`is-monotonically-increasing`. Before this, every one of them died with
"No such method", and `.can` reported False.

The defaults are declared in real Raku source, as ordinary role methods, the
same way rakudo's `Iterator.rakumod` declares them. That source is a new role
prelude in `src/runtime/run_prelude_iterator.rs`. It is injected into any
program or module that composes `Iterator` (`does Iterator`, `but Iterator`),
the way the `Enumeration` prelude is. A compunit that declares its own
`class Iterator` or `role Iterator` does not get it. So the methods compose into the class and compile to bytecode, and a
method the class writes itself still wins. They also appear in `.can` and
`.^methods`. The return values follow rakudo's: `IterationEnd` from the
push/sink methods, and `Int` 1/0 from `skip-one` and `skip-at-least`.

Test: `t/oo/role/iterator-role-default-methods.t`. Closes #9466.
