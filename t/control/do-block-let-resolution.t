use Test;

# GH-7635: `let`/`temp` resolve their saves at the end of the enclosing BLOCK.
# A value-position `do { ... }` IS that block, so it must resolve its own saves
# instead of leaking the speculative value to whatever encloses it. A
# synthesized `Expr::DoBlock` (item context, compound-assignment lowering, the
# chained-comparison desugar, ...) is NOT a block and must keep deferring to the
# real one -- `Expr::DoBlock` carries `DoBlockOrigin` to tell the two apart.
#
# Every assertion below was measured against `raku` first.

plan 21;

# --- a genuine `do { ... }` resolves its own saves -----------------------

{
    my $x = 1;
    do { let $x = 2; Nil };
    is $x, 1, 'do block that fails restores a `let`';
}

{
    my $x = 1;
    do { let $x = 2; 42 };
    is $x, 2, 'do block that succeeds commits a `let`';
}

{
    my $x = 1;
    my $v = do { let $x = 2; Nil };
    is $x, 1, 'do block in value position restores a `let`';
    ok !$v.defined, 'and still yields the block value';
}

{
    my $x = 1;
    do { temp $x = 2; 42 };
    is $x, 1, '`temp` in a do block restores even when the block succeeds';
}

{
    # The inner `do` succeeds, so it COMMITS the save; the outer one failing
    # afterwards must not undo it -- proof that the save resolved at the inner
    # block rather than at the enclosing one.
    my $x = 1;
    do { do { let $x = 2; 42 }; Nil };
    is $x, 2, 'a nested do block resolves at the inner block';
}

{
    my $x = 1;
    L: do { let $x = 2; Nil };
    is $x, 1, 'a labelled do block resolves its own saves';
}

{
    my $x = 1;
    L2: { let $x = 2; Nil };
    is $x, 1, 'a labelled bare block resolves its own saves';
}

# --- statement prefixes whose block runs inline are blocks too -----------

{
    my $x = 1;
    quietly { let $x = 2; Nil };
    is $x, 1, '`quietly BLOCK` resolves its own saves';
}

{
    my $x = 1;
    sink { let $x = 2; Nil };
    is $x, 1, '`sink BLOCK` resolves its own saves';
}

{
    my $x = 1;
    lazy { let $x = 2; Nil };
    is $x, 1, '`lazy BLOCK` resolves its own saves';
}

# --- a statement-position bare block is unchanged ------------------------

{
    my $x = 1;
    { let $x = 2; Nil };
    is $x, 1, 'a statement-position bare block still restores a `let`';
}

{
    my $x = 1;
    { let $x = 2; 42 };
    is $x, 2, 'a statement-position bare block still commits a `let`';
}

# --- a synthesized DoBlock is NOT a block -------------------------------

{
    # Item context `$( ... )` is compiled as an `Expr::DoBlock`, but it opens no
    # scope: the save belongs to the enclosing block, which succeeds here.
    # roast/S04-blocks-and-statements/let.t leans on this shape.
    my $a = 42;
    my $seen;
    { $seen = $( let $a = 23; $a ); 42 };
    is $seen, 23, 'item context sees the speculative value';
    is $a, 23, 'and the enclosing block, having succeeded, commits it';
}

{
    # The same shape with a failing enclosing block: still resolved THERE, so
    # the restore happens -- not at the `$( ... )` wrapper.
    my $a = 42;
    do { $( let $a = 23; $a ); Nil };
    is $a, 42, 'a synthesized wrapper defers resolution to the real block';
}

{
    # The sharp discriminator. The `$( ... )` wrapper's own value is undefined
    # (a failure), the enclosing `do` succeeds. Resolution happens at the `do`,
    # so the save COMMITS; a wrapper that resolved on its own would have
    # restored 42.
    my $a = 42;
    do { $( let $a = 23; Nil ); 42 };
    is $a, 23, 'the wrapper value does not decide the resolution';
}

{
    my $a = 42;
    do { $( temp $a = 23; $a ); 42 };
    is $a, 42, '`temp` through a wrapper still restores at the do block';
}

# --- a string-interpolation block is not one either ----------------------

{
    # `temp` restores unconditionally at the block that OWNS the save, so it is
    # the sharp test for whether a construct is such a block. Measured against
    # `raku`: an interpolation block is not, and the save outlives it.
    my $x = 1;
    my $s = "{ temp $x = 2; 'v' }";
    is $x, 2, 'a string-interpolation block owns no save frame';
}

{
    my $x = 1;
    my $s = "{ let $x = 2; Nil }";
    is $x, 2, 'and so does not roll a `let` back either';
}

# --- the topic is not collateral damage ---------------------------------

{
    # The statement form routes the block value through `$_` to decide
    # success/failure; the value form must not, or `do { ... }` would clobber
    # the enclosing topic.
    my $x = 1;
    $_ = 'topic';
    do { let $x = 2; 99 };
    is $_, 'topic', 'a value-position let block leaves the topic alone';
}
