use Test;

plan 6;

# ADR-0055's dichotomy: an escaping-captured lexical is EITHER authoritative
# (the creating frame vouches for it, so the capture installs with overwrite
# semantics) OR a shared cell. A capture that is ALSO mutated is refused the
# vouch -- correctly, since a by-value install would go stale -- so the cell is
# the only thing left distinguishing it from a same-named lexical in whatever
# frame happens to be calling the closure.
#
# Sub-valued and `&`-sigil bindings fell out of BOTH halves: `ValueView::Sub`
# sat in the value-kind refusal list, `&` was skipped at the boxing site, and
# `&` was excluded from `needs_cell_unvouched_locals` as well. A reassigned
# callable captured by an escaping closure therefore had no defence at all, and
# the closure's free variable resolved by NAME up the live frame chain instead
# of to its own binding.
#
# Found in Algorithm::LCS 0.1.1 (t/01-basic.rakutest): `lcs` assigns its
# `:&compare-i is copy` parameter and hands closures over it to `strip-prefix`,
# whose own parameter is also named `&compare-i` -- so the closure resolved to
# ITSELF and the suite died with a stack overflow before test 1.

# --- `&`-sigil, reassigned, shadowed by a same-named callee parameter --------
{
    my sub inner(&cb, &run) { &run(1) }
    my sub outer() {
        my &cb = -> $x { "FIRST $x" };
        &cb = -> $x { "CORRECT $x" };
        inner(-> $x { "WRONG $x" }, -> $x { &cb($x) });
    }
    is outer(), 'CORRECT 1',
        'a reassigned `&` lexical resolves to its own binding, not the callee parameter';
}

# The self-referential shape that produced the stack overflow: the closure's
# captured `&cb` IS the callee's parameter, so a name resolution recurses.
{
    my sub inner(&cb) { &cb(1) }
    my sub outer(&cb is copy) {
        &cb = -> $x { "base $x" } unless &cb.defined;
        inner(-> $x { &cb($x) });
    }
    is outer(Callable), 'base 1',
        'a closure over an assigned `&` param does not recurse into the callee parameter';
}

# A `$`-sigil lexical HOLDING a Sub is the same hole by a different exclusion.
{
    my sub inner($cb, &run) { &run(1) }
    my sub outer() {
        my $cb = sub ($x) { "FIRST $x" };
        $cb = sub ($x) { "CORRECT $x" };
        inner(sub ($x) { "WRONG $x" }, -> $x { $cb($x) });
    }
    is outer(), 'CORRECT 1',
        'a reassigned Sub-valued `$` lexical resolves to its own binding';
}

# The shadower need not be a parameter -- a nested block`s `my &cb` shadows too.
{
    my sub outer() {
        my &cb = -> $x { "FIRST $x" };
        &cb = -> $x { "CORRECT $x" };
        my &run = -> $x { &cb($x) };
        my $got;
        { my &cb = -> $x { "WRONG $x" }; $got = &run(1); }
        $got;
    }
    is outer(), 'CORRECT 1',
        'a nested block`s same-named `my &` does not hijack the capture either';
}

# The cell must track later mutation -- that is why the vouch refuses a mutated
# capture and the cell is mandatory rather than a by-value snapshot.
{
    my sub outer() {
        my &cb = -> { 'first' };
        my &run = -> { &cb() };
        &cb = -> { 'second' };          # AFTER the capture
        &run();
    }
    is outer(), 'second',
        'the capture observes a mutation made after the closure was created';
}

# Calling the callable back out of the binding still works (the `&` lane has to
# read THROUGH the cell wherever it resolves a name to a callable).
{
    my sub outer() {
        my &cb = -> $x { "v$x" };
        &cb = -> $x { "w$x" };
        my $shape = (&cb.defined, &cb ~~ Callable, &cb(1), cb(2)).join(',');
        my &pass = -> &f { f(3) };
        ($shape, pass(&cb)).join('|');
    }
    is outer(), 'True,True,w1,w2|w3',
        '`&cb` as a value -- .defined, ~~ Callable, call, bare call, passed on';
}
