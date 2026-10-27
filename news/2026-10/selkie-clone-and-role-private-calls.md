# `.clone` twiddles are type-checked, and a role's untaken `self!helper` is not judged at composition

Found through Selkie's store-schema and screen-lifecycle suites.

- `$obj.clone(attr => value)` now assigns through the attribute's type check, so a typed
  attribute refuses a wrong-typed value (`X::TypeCheck::Assignment`), and an `@` attribute
  is bound to an itemized Array as it is (`.clone(tasks => $arr)`), where `new` would wrap
  the item as one element.
- The compile-time check that every `self!name` call names an existing private method no
  longer runs over methods composed from a role: a role cannot know what its composing
  class supplies, and rakudo only checks the class's own methods. Selkie's
  `ScreenModalLifecycle` role calls `self!tween-group` on a path that is not taken.
