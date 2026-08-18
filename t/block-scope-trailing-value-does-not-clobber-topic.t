use Test;

plan 9;

# A block's own trailing statement value must never become `$_`: real raku
# never sets `$_` from a block's own last statement value (`{ 1; 2 }` does not
# make `$_` become `2`). A LEAVE-phaser-bearing bare block, if-branch, or
# given body is compiled through a dedicated `BlockScope` opcode
# (compile_phaser_block_scope) so LEAVE actually fires on any exit -- but
# that path used to route the trailing value through `SetTopic`
# unconditionally, clobbering whatever `$_` the enclosing scope (or a
# `given`'s own topicalized value) had live. Found investigating a CI
# regression on roast/S32-io/open.t's `given open(...) { LEAVE .close; ... }`
# shape. See news/2026-08/given-if-block-scope-topic-clobber.md.

{
    $_ = 'outer';
    { 'x'.chars; LEAVE { is $_, 'outer', 'bare block with LEAVE does not clobber the outer topic' } }
}

{
    $_ = 'outer';
    if True { 'x'.chars; LEAVE { is $_, 'outer', 'if-branch with LEAVE does not clobber the outer topic' } }
}

{
    $_ = 'outer';
    if False { } else { 'x'.chars; LEAVE { is $_, 'outer', 'if-else branch with LEAVE does not clobber the outer topic' } }
}

{
    my $seen;
    given 'x' {
        .chars;
        LEAVE { $seen = $_ }
    }
    is $seen, 'x', "given's LEAVE sees the given topic, not the body's trailing value";
}

{
    my $log = '';
    given (my $fh = 42) {
        .Numeric;
        LEAVE { $log ~= "close:{.raku} " }
    }
    is $log, 'close:42 ', "given's LEAVE topic survives multiple non-trailing method calls on it";
}

# Regression guard: the fix must not break KEEP/UNDO's truthy-value check,
# which depends on actually seeing the block's real trailing value (just not
# via `$_`).
{
    my $str;
    {
        KEEP { $str ~= 'K1 ' }
        UNDO { $str ~= 'U1 ' }
        1;
    }
    is $str, 'K1 ', 'block ending in a truthy value still runs KEEP';
}

{
    my $str = '';
    try {
        KEEP { $str ~= 'K1 ' }
        UNDO { $str ~= 'U1 ' }
        die 'boom';
    }
    is $str, 'U1 ', 'a block that dies still runs UNDO';
}

# Regression guard: a routine's own implicit return value (threaded via the
# topic register internally, since it runs in its own fresh call frame) must
# still work, and must not leak into the caller's `$_`.
{
    $_ = 'outer';
    sub f() { my $y = 5; LEAVE { }; 42 };
    is f(), 42, "a routine's own LEAVE-bearing body still returns its trailing value";
    is $_, 'outer', "a called routine's internal topic threading does not leak into the caller's \$_";
}
