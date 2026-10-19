# The method rows own the implicit `*%_`

Every Raku method carries an implicit `*%_`, so a named argument it does not
declare is swallowed. mutsu used to apply that rule from a hand-kept table,
`accepted_nameds`, which listed the names about eighty methods accept, and the
method-table guard step declined any call passing a name the found row did not
list, leaving the cascades to strip it.

The rows are now the one statement of which names a method binds (ADR-11276
slice 5). The guard step drops a name that no row of the method binds and
declines only when another row binds it (an interpreter twin reading
`unique(:as)`, a `*%unit` row of `later`). The routes that reach a routine by
name strip the same names, read from the same rows, and `accepted_nameds.rs`
is gone.

Declaring them took a survey of Rakudo's signatures for every row
(`scripts/native-method-adverb-survey.raku`): the scheduler `cue`, `DateTime.now`,
`Date.today`, the `Promise` and `Supply` class methods, the socket and
repository methods, the `Supply` and `Proc::Async` instance methods and
`IO::CatHandle`'s readers now list the names Rakudo declares, and the methods
whose `%_` reaches further code (`Grammar.parse`, `Thread.start`,
`Promise.start`, `EVAL`, `Mu.clone`'s twiddles, `later`/`earlier`) bind every
name.
