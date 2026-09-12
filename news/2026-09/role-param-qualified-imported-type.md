# A `::`-qualified imported type is now accepted in a role method's parameter

A nested (`::`-qualified) type imported from a module `use`d inside a role
body was accepted as a parameter type in a `class`/`module`/`package`, but
rejected inside a `role` with `Invalid typename '...' in parameter
declaration.`.

## Root cause

A role method's parameter-type validation runs before the role body's own
`use` statements have loaded (registration happens ahead of the body
running), so a type the `use`d module will eventually supply is not yet
resolvable. The existing accept-heuristic for this deferred the check by
comparing the constraint's name against the `use`d module's own name —
sound only when the two share a textual prefix (`RoleParamImport::Result`
supplied by `use RoleParamImport::Result;`), but unsound in general: nothing
about a type's exported spelling predicts which module supplies it
(`Event::Test`, exported from a module named `Types1` whose own internal
package is `Outer`). A separate rule already existed for exactly this
reasoning on an *unqualified* constraint (defer whenever the body has any
`use`d module not yet loaded, since an unresolvable name really is a typo
only once every such module has loaded) — extended here to qualified
constraints too, since the same reasoning applies identically.

## Test

`t/oo/role/role-param-qualified-type-cross-file-import.t` and its
`-no-smiley` sibling, backed by fixtures under `t/lib/RoleUnrelatedModule*`,
each in their own process (see the follow-up bug below).

## Follow-up work filed separately

Two additional gaps surfaced while investigating and testing this one, out
of scope for this fix and filed as their own issues per the project's
"don't widen the fix" convention:

- [#8083](https://github.com/tokuhirom/mutsu/issues/8083): a role method
  whose parameter names a genuinely mistyped type is silently dropped
  (rather than raising `X::Parameter::InvalidType`) when the accept-deferral
  above applies — this is pre-existing, reproducing before this fix too, for
  an unqualified name.
- [#8084](https://github.com/tokuhirom/mutsu/issues/8084): the same
  qualified type, resolved a *second* time via this deferred path anywhere
  else in the same process (a second method in the same role, or a second,
  independent role importing the same module), mistypes against the value's
  fully-qualified name instead of the relative spelling.

Fixes #8023.
