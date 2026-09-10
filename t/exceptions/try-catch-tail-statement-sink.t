use Test;

# A CATCH/CONTROL phaser occupies a slot in a block's statement sequence for
# TAIL-POSITION purposes, exactly like an ordinary statement -- even though it
# does not run in textual order (it only fires on an exception/control
# signal). So the statement right before it is sunk in place ("Useless use of
# ... in sink context") ONLY when the phaser is textually the LAST thing in
# the block. When the phaser is declared BEFORE the real last statement, that
# statement is still the tail and its value still flows through normally.
# Verified against `raku`:
#   sub f { 42; CATCH { default { } } }; say f();   # Nil (phaser after: sunk)
#   sub f { CATCH { default { } }; 42 }; say f();   # 42  (phaser before: kept)
# mutsu's compiler used to always keep the last real statement's value on the
# stack regardless of the phaser's position, deferring the sink past the
# `TryCatch` opcode's own protected range (or, for a `try {}` used as a bare
# statement, past the whole construct's success/catch merge point) whenever
# the phaser textually followed the statement. So a `return`/other control
# signal raised while sinking that deferred value escaped uncaught, past any
# lexically-enclosing CATCH. See
# todo/deep/return-outside-routine-uncatchable-inside-nested-run.md.
plan 11;

# Direction B1 (the ticket's own repro): the pre-CATCH statement is a
# `EVAL(...)` call whose result is a lazy `gather` -- sinking it in place
# (instead of deferring past the try/catch's protected range) forces the
# gather body, whose escaping `return` is now caught by the CATCH right here.
{
    my $caught;
    my $name;
    try {
        EVAL(q[gather { return  1}]);
        CATCH { default { $caught = .message; $name = .^name; } }
    }
    is $caught, 'Attempt to return outside of any Routine',
        'EVAL(gather{return}) as try tail-before-CATCH is forced and caught';
    is $name, 'X::ControlFlow::Return',
        'the caught exception has the right type';
}

# Plain literal tail statement AFTER which an explicit CATCH is declared:
# sunk, block yields Nil/Any, not the literal.
{
    my $x = try { 42; CATCH { default { } } };
    ok !$x.defined, 'try { 42; CATCH {} } discards the tail literal (yields Any)';
}

# Same rule for CONTROL (no CATCH at all), phaser after.
{
    my $x = try { 42; CONTROL { } };
    ok !$x.defined, 'try { 42; CONTROL {} } discards the tail literal (yields Any)';
}

# Same rule for a bare `do` block (not a genuine `try`) with CATCH -- the
# implicit-try wrapper path (`compile_implicit_try`), not `compile_try`.
{
    my $x = do { 42; CATCH { default { } } };
    ok !$x.defined, 'do { 42; CATCH {} } discards the tail literal (yields Any)';
}

# A `try {}` with NO catch/control at all is unaffected: the tail value still
# flows through normally.
{
    my $x = try { 42 };
    is $x, 42, 'try { 42 } with no CATCH/CONTROL still yields the tail value';
}

# CATCH declared BEFORE the tail statement: the tail value is KEPT, not sunk.
# This is the regression case a naive "any CATCH anywhere discards the tail"
# fix would break (it did, once, during development of this fix -- caught by
# t/catch-block-implicit-return.t and t/catch-block-keeps-block-value.t).
{
    sub f() { CATCH { default { } }; 42 }
    is f(), 42, 'CATCH declared BEFORE the tail statement keeps the tail value';
}

# CATCH in the middle, with more real statements after it: still kept, since
# nothing textually follows the true last statement.
{
    sub f() { my $x = 1; CATCH { default { } }; $x }
    is f(), 1, 'CATCH declared in the middle (real statements follow) keeps the tail value';
}

# A sub body ending with CATCH (phaser after) discards its would-be-implicit-
# return tail value.
{
    sub f() { 42; CATCH { default { } } }
    my $r = f();
    ok !$r.defined, 'sub f { 42; CATCH {} } (phaser after) returns Nil, not the tail literal';
}

# A tail statement that is itself a call (compile_tail_stmt_call_value path,
# not the plain Expr path) is sunk the same way when the phaser follows it.
{
    sub helper() { 99 }
    my $x = try { helper(); CATCH { default { } } };
    ok !$x.defined, 'a tail call statement before a trailing CATCH is sunk (yields Any)';
}

# A block with a LEAVE phaser (no CATCH/CONTROL) already discarded the tail
# value via a separate, pre-existing mechanism (`has_block_enter_leave_phasers`)
# -- pinned here so both mechanisms are known not to conflict.
{
    my $x = try { 42; LEAVE { } };
    ok !$x.defined, 'try { 42; LEAVE {} } discards the tail literal too';
}
