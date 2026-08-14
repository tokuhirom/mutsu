use Test;

# A Rust-level panic (integer overflow, index OOB, capacity overflow, ...)
# raised deep inside a nested closure/sub call is converted to a catchable
# X::AdHoc at the enclosing `try`/`EVAL` boundary (see t/vm-panic-boundary.t).
# But the conversion only ran `catch_unwind` around the *outermost* boundary;
# every call frame pushed between that boundary and the panic site normally
# gets popped by its own return-path cleanup (`pop_call_frame`), which a Rust
# unwind skips entirely (only `Drop` runs on unwind, and the pop is plain
# code). So `self.call_frames` was left holding every frame pushed since the
# boundary, and `self.locals`/`self.upvalues`/`self.env` still belonged to the
# deepest panicking callee instead of the code resuming at the boundary.
#
# The code resuming after the `try` indexes `self.locals` with slot numbers
# valid for ITS OWN locals array, not the leftover (typically much smaller)
# callee one -- an immediate secondary "index out of bounds" panic reading an
# ordinary local variable, which used to abort the whole program even though
# the *original* panic was supposed to be safely caught.
#
# Repro shape: invoke a panicking block through a Callable indirection
# (`$code()`), inside a `try {}`, so the closure call pushes exactly one call
# frame before panicking -- matching how the vendored Test.rakumod's
# `dies-ok`/`lives-ok` invoke the tested block (`$code()`), which is how this
# was originally found (t/vm-panic-boundary.t under MUTSU_REAL_TEST=1).

plan 4;

{
    my $code = -> { my @a; @a[2**64 - 1] = 1 };
    my $before = 'untouched';
    my $after;
    try {
        $code();
        $after = 'ran to completion';
    }
    # Reading $before here is the crash site pre-fix: `self.locals` was still
    # the panicking closure's own (much smaller) array, so this GetLocal
    # indexed out of bounds and the whole process aborted.
    is $before, 'untouched', 'a local declared before the try is still readable after a nested-call panic';
    ok !$after.defined, 'the try body did not run past the panic';
}

{
    # Same shape, two levels of Callable indirection, so two call frames are
    # pushed (and both need recovering) before the panic.
    my $inner = -> { my @b; @b[2**64 - 1] = 1 };
    my $outer = -> { $inner() };
    my $x = 1;
    my $y = 2;
    try {
        $outer();
    }
    is $x + $y, 3, 'locals survive a doubly-nested-call panic';
}

{
    # A panic caught by an inner try must not corrupt state visible to an
    # outer try/CATCH either.
    my $marker = 'start';
    try {
        try {
            my $code = -> { my @a; @a[2**64 - 1] = 1 };
            $code();
        }
        $marker = 'inner try completed';
        die "expected failure after inner try";
        CATCH { default { $marker = 'outer caught: ' ~ .message } }
    }
    is $marker, 'outer caught: expected failure after inner try',
        'state stays coherent across nested try boundaries after a panic';
}
