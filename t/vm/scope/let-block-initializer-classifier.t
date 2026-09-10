use Test;

# GH-7645: `Compiler::has_let_deep` decides whether a block gets an
# `OpCode::LetBlock` save frame. It walked statements that hold an expression
# directly (`Stmt::Expr`, `Stmt::Say`, ...) but not a declaration's or an
# assignment's initializer, so a `let`/`temp` reached only through one was
# invisible and NOTHING resolved the save -- the speculative value became
# permanent instead of being rolled back at the enclosing block.
#
# The companion half is that a `let` block is now the plain block PLUS a save
# frame (the frame nests inside `OpCode::BlockScope`) rather than a different
# kind of block that skips the env restore.
#
# Every expectation below was measured against `raku` first.

plan 14;

# --- a `let` in a declaration's initializer resolves at the block ---------

{
    my $a = 42;
    { my $seen = $( let $a = 23; $a ); Nil };
    is $a, 42, 'a `let` in a declaration initializer is rolled back by a failing block';
}

{
    my $a = 42;
    my $seen;
    { my $s = $( let $a = 23; $a ); $seen = $s; 42 };
    is $a, 23, 'and committed by a succeeding one';
    is $seen, 23, 'the initializer still sees the speculative value';
}

{
    my $a = 42;
    { my $s = $( temp $a = 23; $a ); 42 };
    is $a, 42, '`temp` in a declaration initializer restores even on success';
}

# --- the same through a plain assignment ---------------------------------

{
    my $a = 42;
    my $seen;
    { $seen = $( let $a = 23; $a ); Nil };
    is $a, 42, 'a `let` in an assignment initializer is rolled back by a failing block';
    is $seen, 23, 'and the assignment still saw the speculative value';
}

{
    my $a = 42;
    my $t = 0;
    { $t += $( let $a = 23; 1 ); Nil };
    is $a, 42, 'a `let` inside a compound assignment is rolled back too';
    is $t, 1, 'and the compound assignment still ran';
}

# --- a genuine `do { ... }` still resolves at itself ----------------------

{
    # The inner `do` IS a block (GH-7635), so it commits its own save and the
    # failing outer block must not undo it.
    my $a = 42;
    { my $s = do { let $a = 23; 42 }; Nil };
    is $a, 23, 'a `do` block in an initializer still resolves at the `do`';
}

# --- a `let` block keeps the scope semantics of a plain block -------------

{
    my $x = 42;
    { let $x = 1; my $*dyn = 'inner'; Nil };
    is $x, 42, 'the `let` still rolls back';
    nok $*dyn.defined, 'a `my $*dyn` declared in a `let` block does not leak out';
}

{
    my $x = 42;
    { let $x = 1; my $*dyn2 = 'inner'; 99 };
    is $x, 1, 'a succeeding `let` block still commits';
    nok $*dyn2.defined, 'and its `my $*dyn` is block-scoped as well';
}

{
    my $x = 42;
    my $shadowed = 'outer';
    { let $x = 1; my $shadowed = 'inner'; Nil };
    is $shadowed, 'outer', 'a shadowing `my` in a `let` block does not leak either';
}
