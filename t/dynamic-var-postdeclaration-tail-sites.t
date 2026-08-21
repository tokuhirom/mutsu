use v6;
use Test;

# X::Dynamic::Postdeclaration must fire for a tail-position `my $*x := ...`
# declaration (parsed as `Stmt::SyntheticBlock([MarkReadonly/MarkBind,
# VarDecl])`) reached through ANY compile path that hand-inlines a
# block-final/tail-position/expression-position `VarDecl`, not just the one
# call site (`compile_block_inline`'s own tail arm) originally patched.
#
# Regression: several other tail-position dispatch sites recursively called
# `compile_block_inline` on the wrapper's inner statements to get "declare and
# yield the value" semantics, but reached `Stmt::VarDecl` through a hand-
# inlined tail-position compile arm that never had the
# X::Dynamic::Postdeclaration / X::Dynamic::Package checks the *ordinary*
# (non-tail) `Stmt::VarDecl` arm has always had -- so the checks silently
# never fired through those paths.

plan 8;

# Site: `compile_unit`'s top-level mainline tail-statement dispatch
# (src/compiler/mod.rs) -- a top-level program whose LAST statement is a
# `my $*x := ...` bind reached through the recursive `compile_block_inline`
# call for a tail `Stmt::SyntheticBlock`.
throws-like 'say $*POSTDECL2 // "x"; my $*POSTDECL2 := 1;',
    X::Dynamic::Postdeclaration, symbol => '$*POSTDECL2',
    'a top-level mainline program whose last statement is a `:=` bind still throws (mod.rs top-level tail dispatch)';

# Site: `compile_expr`'s `Stmt::SyntheticBlock` arms in expression position
# (src/compiler/expr_block.rs) -- a `:=` bind used as a sub-expression, e.g.
# inside a numeric coercion, reaches the `MarkBind` synthetic-block arm.
throws-like 'do { say $*POSTDECL3 // "x"; +(my $*POSTDECL3 := 1) }',
    X::Dynamic::Postdeclaration, symbol => '$*POSTDECL3',
    'a `:=` bind used as an expression-position sub-expression still throws (expr_block.rs SyntheticBlock arm)';

# Site: `given`/`when` tail-statement dispatch (`compile_when_tail_stmt` in
# src/compiler/helpers_block_inline.rs) -- a `given` block used as an
# expression whose last statement is a `my $*x := ...` bind. `given` pushes
# its own lexical dynamic-var scope, so the read must be INSIDE the given
# body (same block) to exercise the same-block check through this path.
throws-like 'my $r = do given 1 { say $*POSTDECL4 // "x"; my $*POSTDECL4 := 2; }',
    X::Dynamic::Postdeclaration, symbol => '$*POSTDECL4',
    'a `given`-as-expression whose tail statement is a `:=` bind still throws (compile_when_tail_stmt)';

# Site: value-collecting block-body dispatch (`compile_stmts_value` in
# src/compiler/helpers_control_flow.rs) -- a `for` loop used to collect
# values whose body's last statement is a `my $*x := ...` bind. `for` also
# pushes its own lexical dynamic-var scope per iteration, so the read must be
# INSIDE the loop body.
throws-like 'my @r = do for 1..2 { say $*POSTDECL5 // "x"; my $*POSTDECL5 := $_ }',
    X::Dynamic::Postdeclaration, symbol => '$*POSTDECL5',
    'a value-collecting `for` body whose tail statement is a `:=` bind still throws (compile_stmts_value)';

# Site: a sub's own body tail-statement dispatch
# (src/compiler/helpers_sub_body.rs) -- a fresh `Compiler` compiles each
# routine body, and (like the top-level mainline) never pushes its own
# dynamic-var scope around the body itself, so it has the identical gap as
# the mainline site above.
throws-like 'sub g() { say $*POSTDECL6 // "x"; my $*POSTDECL6 := 1; }; g();',
    X::Dynamic::Postdeclaration, symbol => '$*POSTDECL6',
    'a sub body whose last statement is a `:=` bind still throws (helpers_sub_body.rs tail dispatch)';

# Regression guard: a read INSIDE a nested `do {}` (its own real lexical
# scope) must not leak out and poison an UNRELATED top-level tail `:=` bind
# of the same name -- the mainline fix above must stay scoped to genuine
# same-block cases, not turn into an unscoped whole-program check again.
{
    my $seen = do { $*UNRELATED // 'none' };
    my $*UNRELATED := 99;
    is $seen, 'none', 'a read inside a closed nested do{} does not poison an unrelated top-level tail := bind';
}

# Same regression guard through the sub-body tail dispatch fixed above.
{
    sub h() {
        my $seen = do { $*SUBUNRELATED // 'none' };
        my $*SUBUNRELATED := 99;
    }
    is h(), 99, 'a read inside a closed nested do{} does not poison an unrelated sub-body tail := bind';
}

# Regression guard: the existing scope-boundary fix (`do { my $*CUR := 42; }`
# read outside, declared inside a nested do{}) must still be legal when that
# do{} is itself the LAST statement of a sub body (routes through the
# top-level/tail-position dispatch fixed above, not just statement position).
{
    sub go() {
        my @seen;
        @seen.push($*CUR2 // 'none');
        do {
            my $*CUR2 := 42;
            @seen.push($*CUR2 // 'none');
        }
        @seen;
    }
    is-deeply go(), ['none', 42],
        'an outer read before an inner do{}-scoped := bind (tail sub body) is still legal';
}
