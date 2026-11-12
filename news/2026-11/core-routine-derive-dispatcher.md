# A built-in routine can be extended through a derived dispatcher

In rakudo, every built-in routine is a proto. `&postcircumfix:<{ }>.clone.derive_dispatcher`
gives a dispatcher of its own. Candidates added to it with `add_dispatchee`
answer first, and the built-in answers the rest. mutsu now supports this too:

- A built-in routine's `.is_dispatcher` is `True`.
- `.derive_dispatcher` works on a built-in routine and on any dispatcher.
- A `sub EXPORT` that returns such a dispatcher for a subscript operator
  overrides the `{ }` / `[ ]` subscript in the block that `use`s the module.
  Outside that block the subscript is unchanged.
- A BEGIN-time `$*R.resolve-lexical-constant('&name')` finds a routine,
  including a built-in one. With `:current-scope-only` it finds only what the
  current scope declares.

This is what CompUnit::Util's `push-lexical-multi` relies on. With it,
CompUnit::Util 0.7.0's `t/09-push-multi.rakutest` passes 15/15, as on rakudo
(#15371).
