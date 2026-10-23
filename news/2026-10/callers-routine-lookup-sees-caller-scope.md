# `CALLERS::<&name>` and `CALLERS::OUTERS::<&name>` see the caller's lexical scope

`::CALLERS::<&name>` looked a routine up in the caller frames' pads and the
running unit's own resolution only, so a routine the caller's unit got from a
top-level `use` of a `sub EXPORT` hook module (or declared around the caller, in
its file or class body) was invisible: `CALLERS::` found nothing, and
`CALLERS::OUTERS::` was answered against the *reader's* frame. Both now walk the
calling routines' lexical scopes (the unit's import sets, then the routines its
package chain declares), with `OUTERS` leaving out the program's mainline, as
rakudo does. A routine body that carries a `CATCH` phaser is no longer taken
for an immediate block either, which routed its `CALLERS::` read through the
lexical `OUTERS` walk instead of the real call stack.

Candidates of a built-in operator that a `sub EXPORT` hook hands out
(`'&prefix:<!>' => ...`) now add to the operator like an `is export` import
does: `!` of a Failure or of an unresolved dynamic variable no longer dies with
"Cannot resolve caller prefix:<!>" once `use Red` is in effect.

Together these clear the `X::Red::NoOperatorsDefined` / "falling back" cluster
of the Red 0.2.7 distribution (#14094).
