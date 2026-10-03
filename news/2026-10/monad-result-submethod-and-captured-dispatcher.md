# Monad::Result: same-named submethod and sub, captured multi dispatchers

Drawing `Monad-Result` from the ecosystem roulette turned up two bugs. Before
these fixes the module could not even be loaded; now both of its test files
pass 9/9, matching rakudo.

- **A submethod no longer takes the name of a same-named `our sub`.**
  `submethod ok` next to `our sub ok` in one class died with "Redeclaration of
  routine 'ok'". The class-body registration treated the submethod like a
  lexical `my method` (the parser marks both `is_my`) and installed it as a
  function under the class's name. A submethod is only a method now, in both
  `class` and `augment` bodies.
- **A captured multi dispatcher keeps its own candidates.** The tests take
  `my ($ok, $plan) = do { use Test; (&ok, &plan) }` and then import
  `Monad::Result :subs`, whose single `ok` takes one argument. Calling the
  captured `$ok` dispatched by name, found that later single `ok` and died
  with "Too many positionals". A captured dispatcher now hands the call to a
  live routine of the same name only when that routine is one of its captured
  candidates.
