# A compunit's `trait_mod:<is>` candidates no longer reach the modules it loads

A `multi` is hoisted to the start of its block, so a compunit's
`multi trait_mod:<is>` candidates are registered before its own `use`
statements run. mutsu scoped a module's package-less multi families to that
module only after its load had finished, so while it was loading, its
candidates took part in the trait dispatch of every module it loaded in turn.
Upstream `NativeCall.rakumod` has this shape: its `is symbol` / `is mangled`
candidates captured the `is array_type(...)` traits of `NativeCall::Types`,
and loading it failed (#11310).

What changed:

- **Scoping.** Before a nested module load, the importer's families declared so
  far are scoped to the importer. For the main script only its
  `trait_mod:<...>` families are scoped; its other multis keep the unit-blind
  dispatch caches.
- **`is array_type(T)` is a core trait.** It is never dispatched to a user
  candidate.
  - A class records it, and `.^array_type` / `.^set_array_type` read and write
    it.
  - A role's trait applies to each class that composes the role, with the
    role's type arguments bound. A class's own trait wins.
- **No accepting candidate.** An `is foo(...)` that no candidate accepts is now
  `X::Inheritance::UnknownParent`, as in rakudo, whether or not any user
  candidate exists.
- **Prelude gates.** The native NativeCall provider's preludes are now switched
  on by `NativeCall` as a whole name. A longer identifier such as
  `NativeCallSymbol` no longer counts.

`scripts/nativecall-upstream-trial.sh` now gets through `load UNC` as far as
`nqp::nativecallsizeof`, which is #11211.
