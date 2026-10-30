# Operators bound into `EXPORT::MANDATORY`, `our only`, `+ [..]`, `%@array`

Working the `annotations` distribution (#15259) closed five parse/runtime gaps on the way to its
`use annotations <declare direct class>` operators:

- `OUR.WHO.BIND-KEY: .key, .value for Other::` inside an export stash package (`package EXPORT {
  package MANDATORY { ... } }`) publishes the copied routines as the module's exports, and flags the
  module for the parse-time probe so `1 graffiti 2` parses as an infix. The probe also keeps the
  names a module bound before its argument-taking `EXPORT` hook died.
- A package stash (`P::.keys`) lists its `our` operator routines (`&infix:<zz>`), so a stash can be
  copied key by key.
- `our only name(...)` / `my only sub name(...)` declare a routine; `module M { our only f }` is
  reachable as `M::f`.
- `+ [$a, $b]` / `+ ($a, $b)` (a space before the sub-signature of the anonymous one-arg slurpy).
- `%@array` is hash context on an array variable, `@%hash` list context on a hash.
- `Mod::Name` read from inside `my package EXPORT { ... }` finds a module whose members were stored
  with `OUR::<Name> := ...`.

The `annotation` declarator itself (`EXPORTHOW::DECLARE` from a runtime `EXPORT` hook) remains.
