# A sibling grammar's implicit parent is the core Grammar, not a module-local `grammar Grammar`

A grammar declared with no explicit `is` clause always inherits the *core*
`Grammar` Cursor. This holds even inside a `module` that also declares its own
`grammar Grammar`: a second, sibling grammar in the same module must inherit the
built-in Grammar, never the user grammar that merely happens to be named
`Grammar`.

mutsu got this wrong. `qualify_sibling_parent_name` rewrites a bare inheritance
parent to a package-qualified sibling (`X` -> `M::X`) so that
`class X::Decode is X` inside `module M` links to `M::X` rather than the built-in
`X::` exception namespace. But it also rewrote the implicit `Grammar` parent the
parser auto-adds to every grammar. Inside a module `M` that had already declared
`grammar Grammar` (registered as `M::Grammar`), a later `grammar Schema` had its
default `Grammar` parent qualified to `M::Grammar` and so inherited that user
grammar's tokens, its `Actions` class, and — most damagingly — its `method parse`
override. Calling `Schema.parse(...)` then re-dispatched through the main
grammar's `parse` and reduced with the *wrong* Actions, so a scalar result came
back wrapped/lost instead of the value `Schema`'s own `{ make ... }` produced.

The fix excludes `Grammar` from `qualify_sibling_parent_name`: a bare `Grammar`
parent stays the core Grammar. Direct references (`Grammar.parse`) still resolve
to the module-local grammar via bare-word resolution, so the module-local shadow
(the earlier "`grammar Grammar` in a module is a real grammar" fix) is intact —
only the implicit inheritance parent is affected.

This was the last blocker on the scalar path of the **YAMLish** battery: with
the correct parent, `Schema::JSON` / `Schema::Core` stop inheriting the 780-line
main `Grammar` and reduce their own element tokens, so
`use YAMLish; load-yaml("42")` now yields `42` (previously `Any`). Block
collections (sequences, maps) are a separate, still-open parsing frontier.

Pins: `t/grammar-sibling-implicit-core-parent.t` (+ `t/lib/GrammarSiblingCore.rakumod`).
