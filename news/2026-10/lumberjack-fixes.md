# Match destructuring, rw-method containers, numeric ACCEPTS, type-object aggregate assignment

Four gaps found by the ecosystem roulette on Lumberjack:

- A `Match` destructures through its `.Capture`: `-> ( :$specifier ) { ... }`
  as an `.subst` replacement binds the named capture, and `-> ($a, $b)` the
  positional ones. mutsu bound named sub-parameters from the Match object's
  own attributes.
- An `is rw` method returning an outer lexical (`method level() is rw
  { $level }`) can be assigned from a frame that has its own readonly
  `$level` (a parameter or loop variable). That caller's mark leaked into the
  method by name; the method now reconciles the free variables it reads, not
  only those it writes, against its declaring scope.
- `$obj ~~ 2` / `$obj ~~ SomeEnumValue` compares numerically through the
  object's own `.Numeric`, as `Numeric.ACCEPTS` does.
- `Class.method = (...)` assigns into the `Array`/`Hash` the method returns
  (Lumberjack's `Lumberjack.dispatchers = (...)`), instead of refusing a
  non-instance invocant.

Seven of Lumberjack's eight test files now pass under mutsu; `t/050-formatter.t`
needs #11292 (a module's `my regex` interpolating a module lexical).
