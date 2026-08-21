# Fix a false X::Dynamic::Postdeclaration for an unrelated later `my $*x` in a different scope

A dynamic variable (`$*x`) read in an outer/earlier scope, followed later by
an unrelated inner-scoped `my $*x := ...` declaration in a nested block,
wrongly made mutsu think the earlier read was "before declaration" and threw
`X::Dynamic::Postdeclaration`. Real Raku treats these as unrelated: the
inner `my $*x` is scoped to its own block and does not retroactively apply
to the earlier read.

```raku
class Foo {
    method go(&task) {
        say $*CUR // 'none';
        do {
            my $*CUR := 42;
            task();
        }
    }
}
Foo.new.go(-> { say $*CUR // 'none' });
```

- raku: `none` then `42`.
- mutsu (before the fix): printed `none`, then died with
  `X::Dynamic::Postdeclaration with no message`.

## Root cause

The compiler tracked "has `$*x` ever been read anywhere in this routine" as
a single unscoped `HashSet<String>` (`Compiler::accessed_dynamic_vars`),
populated whenever a `$*x`-style variable was compiled as a read
(`expr_helpers.rs`) and consulted when compiling a `my $*x` declaration
(`stmt.rs`). Because the set was never cleared per block, ANY earlier read
of `$*x` anywhere in the enclosing routine — not just in the SAME lexical
block as the declaration — poisoned every later `my $*x` in that routine,
including one in a completely unrelated nested or sibling scope.

Checked against `raku`: the real rule is narrower. `my $*x` after an
earlier `$*x` read is illegal ONLY when both are in the exact same
immediate lexical block; a read in an enclosing/sibling scope, or in a
nested block that closed before the `my` (which is itself in an
outer/sibling block), is legal and resolves through the ordinary dynamic-var
lookup chain.

## Fix

`accessed_dynamic_vars` is now scoped like the compiler's other
per-block state (`my_vars_current_scope`, `constant_vars_current_scope`):
reset on `push_dynamic_scope_lexical` (block entry) and restored on
`pop_dynamic_scope_lexical` (block exit), so a read recorded inside a
nested block never leaks back out to an enclosing declaration, and a read
in an enclosing scope is invisible to an unrelated inner declaration.

Verifying the true-positive case (`do { say $*CUR; my $*CUR := 42; }` in
the SAME block, which must still throw) surfaced a second, narrower,
pre-existing gap: when the `my $*x := ...` declaration is the block's LAST
(tail-position) statement, it is parsed as `Stmt::SyntheticBlock([MarkReadonly,
VarDecl])` and compiled through a separate, hand-inlined tail-position
`VarDecl` compile arm in `compile_block_inline` that never had the
`X::Dynamic::Postdeclaration` / `X::Dynamic::Package` checks the ordinary
(non-tail) `Stmt::VarDecl` arm has always had — so the genuine error was
silently skipped whenever the declaration happened to be block-final. Fixed
by extracting the checks into a shared
`Compiler::check_dynamic_var_decl_errors` helper (in the new
`src/compiler/helpers_dynamic.rs`) and calling it from both compile arms,
plus a `next_dynamic_scope_inline_transparent` one-shot flag so the
recursive `compile_block_inline` call used to inline a `SyntheticBlock`'s
body does not reset `accessed_dynamic_vars` mid-check (a `SyntheticBlock` is
a parser wrapper, not a real lexical scope — its direct, non-tail dispatch
already inlines with no push/pop at all).

Several OTHER call sites that can also reach a tail-position `my $*x := ...`
(the mainline top-level compile loop, several expression-position
`SyntheticBlock` arms) were found to have the same gap but were left
unpatched to keep this fix narrowly scoped and well-tested; recorded as
`todo/tickets/dynamic-var-postdeclaration-tail-synthetic-block-skips-check.md`
for a follow-up sweep.

Verified against `raku` as the oracle for the false-positive repro, the
genuine same-block error (both non-tail and tail position), and a read in a
nested block predating an outer declaration. Also confirmed end-to-end
against the real-world motivating case,
`modules/Log-Timeline`'s `Log::Timeline::Task`'s `multi method log(&task,
*%data)` (`with PROCESS::<$LOG-TIMELINE-OUTPUT> { ...; do { my
$*LOG-TIMELINE-CURRENT-TASK := $ongoing; task() } }`), which now runs
correctly under mutsu.

New regression coverage: `t/dynamic-var-postdeclaration-scope-boundary.t`.
