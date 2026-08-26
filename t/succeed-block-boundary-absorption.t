use Test;

# A `when`/`default` succeed unwinds to the nearest enclosing topicalizer
# (`given`/`with`) if there is one; otherwise Raku absorbs it at the nearest
# enclosing block-like construct instead of crashing: a bare block, an `if`
# branch, a loop body, a `try`, a sub body, or -- with nothing else left at
# all -- the compilation unit itself. mutsu used to have no absorber for a
# `when` reached through an EXPRESSION (`do when COND { ... }` nested inside
# an assignment, a call argument, a list, ...) rather than appearing as a
# literal top-level `when` statement, because the compiler's
# `body_has_toplevel_when` scan only looked for the literal `Stmt::When`
# shape. `do when` is an ordinary term and can appear at any expression
# nesting depth, so that scan was fundamentally incomplete; the fix makes
# every one of these absorbing boundaries unconditional instead
# (`SucceedBarrier` in `compiler/stmt.rs`/`compiler/helpers_control_flow.rs`,
# the mainline catch in `runtime/run.rs`, and a dedicated arm in
# `vm/vm_try_catch_ops.rs` for `try`, which turned out to have never
# absorbed a `when`'s succeed at all -- see
# news/2026-08/succeed-absorbing-block-boundary.md).
#
# The "matching `when` yields its body's value" intuition is a trap: a
# MATCHING `when` never lets that value flow into a pending assignment --
# it runs the block and then raises succeed, which abandons the assignment
# entirely. So `$a = do when .so { "foo" }` leaves `$a` at its declared
# default (`Any`) whenever the `when` matches, in both raku and mutsu; only
# the NON-matching path assigns the (falsy) smartmatch result.

plan 15;

# --- bare block, no topicalizer ---

{
    $_ = True;
    my $a;
    { $a = do when .so { "foo" } }
    is $a, Any, 'bare block absorbs a matching when; pending assignment never completes';
}

{
    $_ = False;
    my $a;
    { $a = do when .so { "foo" } }
    is $a, False, 'bare block: non-matching when assigns the falsy smartmatch result';
}

# --- given topicalizer (already correct before this fix; pinned so it does
# not regress) ---

{
    my $a;
    given True { $a = do when .so { "foo" } }
    is $a, Any, 'given absorbs a matching when the same way';
}

{
    my $a;
    given False { $a = do when .so { "foo" } }
    is $a, False, 'given: non-matching when assigns the falsy smartmatch result';
}

# --- if branch, no enclosing given ---

{
    $_ = True;
    my $a;
    if 1 { $a = do when .so { "foo" }; say "if-inner-unreached" }
    is $a, Any, 'an if branch (no block-local `my`) also absorbs the succeed';
}

# --- sub body: the succeed becomes the sub's own return value ---

{
    my @log;
    sub f() {
        $_ = True;
        my $a = do when .so { "foo" };
        @log.push("unreached");
        return "ret";
    }
    is f(), "foo", 'a sub body absorbs an escaping succeed as its own return value';
    is @log.elems, 0, 'statements after the escaping succeed never run';
}

# --- for loop body: succeed ends the whole loop, like `last` ---

{
    my @seen;
    for 1, 2, 3 -> $x {
        $_ = True;
        my $a = do when .so { "x=$x" };
        @seen.push($x);
    }
    is @seen.elems, 0, 'succeed with no topicalizer ends a for loop entirely, not just one iteration';
}

# --- try: absorbs the succeed like a bare block, continuing after `try` ---

{
    my @log;
    try {
        $_ = True;
        my $a = do when .so { "foo" };
        @log.push("try-inner-unreached");
    }
    @log.push("after-try");
    is @log.join(","), "after-try",
        'try absorbs an escaping succeed and execution continues after the try';
}

# --- try nested inside given: try is the NEARER boundary, so the outer
# given's own body keeps running past the try statement ---

{
    my @log;
    given 5 {
        try { when 5 { @log.push("matched-in-try") } }
        @log.push("after-try-in-given");
    }
    @log.push("after-given");
    is @log.join(","), "matched-in-try,after-try-in-given,after-given",
        'a when matching inside a try nested in a given is absorbed by the try, not the given';
}

# --- sanity: given directly wrapping when still ends the given body early
# (this must NOT regress: the try case above must not make every given
# swallow its own body's succeed too) ---

{
    my @log;
    given 5 {
        when 5 { @log.push("matched") }
        @log.push("after-when-in-given-unreached");
    }
    @log.push("after-given");
    is @log.join(","), "matched,after-given",
        'given directly wrapping a matching when still ends its own body early';
}

# --- regression pin for the pre-existing literal-top-level-when case this
# fix's mechanism replaces (see compiler/stmt.rs's SucceedBarrier comment) ---

{
    my @log;
    given 5 {
        { when Int { @log.push("matched-in-block") } }
        @log.push("after-block-in-given");
    }
    is @log.join(","), "matched-in-block,after-block-in-given",
        'a when nested in a bare block inside a given still lets the given continue past the block';
}

# --- deep expression nesting: do-when reachable through a list literal /
# a call argument, still absorbed at the bare block boundary ---

{
    $_ = True;
    my @log;
    {
        my @a = (1, do when .so { 2 }, 3);
        @log.push("inner-unreached");
    }
    @log.push("outer");
    is @log.join(","), "outer",
        'a do-when nested arbitrarily deep in an expression is still caught by the block boundary';
}

# --- mainline with nothing enclosing at all: the compilation unit itself is
# the terminal boundary. This aborts the WHOLE program, so it must run in a
# subprocess. ---

{
    my $p = run $*EXECUTABLE, '-e',
        '$_ = True; my $a = do when .so { "foo" }; say $a;',
        :out, :err;
    is $p.exitcode, 0,
        'a mainline-level succeed with no enclosing construct exits cleanly, not a crash';
    is $p.out.slurp(:close) ~ $p.err.slurp(:close), '',
        'the mainline ends silently right at the succeed -- the trailing say never runs';
}

done-testing;
