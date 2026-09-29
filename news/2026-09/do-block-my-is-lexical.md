# A `my` inside a value-position `do { }` block no longer leaks

`my $x = do { my $zz = 9; 1 }; say $::("zz")` printed `9`: `OpCode::DoBlockExpr`
left every declaration of a value-position block in the enclosing env, so a
symbolic lookup after the block still found it. Rakudo treats `do { ... }` as a
block, and the name is gone once it exits.

`DoBlockExpr`'s boolean `scope_isolate` is now a three-way `DoBlockIsolation`.
Every value-position node the parser mints from real source braces
(`DoBlockOrigin::SourceBlock`: `do { }`, a labelled block, the inline `lazy` /
`sink` / `quietly` blocks) takes the new `Lexical` policy. It reverts exactly the
block's own declarations on exit, whatever their sigil, together with their type
metadata and the `$*x` mirror of a `my $*x`. Every other write persists: an outer
lexical, a dynamic variable, `$!`, `$/`, and closures escaping the block. The
compile-time list of a block's declarations now covers only its own scope, so a
nested block's `my $x` no longer reverts a later write to the outer `$x`.
Desugared statement lists (`$( ... )`, the parser's `SyntheticBlock` wrappers)
are not blocks and still declare into the enclosing scope.

The fix exposed a bug that the leak had been hiding. A `method` in a role body's
`do { }` block (`role R { do { my $v = 9; method m { $v } } }`) keeps its lexical
capture, but `apply_nested_method_captures` only handed that capture to
`registry.roles`. A mixin (`1 but R`) reads the role from its `role_candidates`
copy, so it used to find `$v` only because the `my` had leaked into the role's
package lexicals. The capture now reaches that copy too.

ADR-0076 §6 records the fix. Merging the two block-scope opcodes is still
deferred. Pinned by `t/control/do-block-my-is-lexical.t` (#9897).
