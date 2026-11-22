# `$obj but R` always composes a real mixin type

`$obj but R` on an instance had two implementations: a copy reblessed into a
registered mixin class `C+{R}` (rakudo's `.clone.does(R)`, also what an
in-place `$obj does R` uses), taken only for a parametric role or one with
attribute-trait hooks, and the older mixin-map wrapper for every other role.
Every `but` on an instance now reblesses; the wrapper is left only for the
values the rebless cannot take (built-in classes and roles with no registry
entry, internal descriptor objects) (#17890).

The last gaps the reblessed type had against the wrapper are closed:

- A closure a role method made reads and writes the copy of `$!a` that its
  role's mixin level keeps when two mixed-in roles both declare `$!a`
  (`$o does N; $o does P; $o.mk()()` reads N's, as rakudo does). The closure
  names its role; the access maps the role to the MRO level class that
  composed it, in the shared resolution and in the rw-container path (#17936).
- `.^set_name` renames the type for display too: a renamed type object gists
  and stringifies under its new name wherever it is reached (`A.gist`,
  `.^mro.gist`, every object of a renamed mixin type), through a
  process-global rename table the display layer reads, as it does for
  package-scoped enum names.
