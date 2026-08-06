use Test;

# A `&`-sigil lexical (a `my &name` binding, or a `&name`/`:&name` Callable
# parameter) must shadow a same-named builtin for a bare call `name(...)` --
# resolution goes by "is something declared here", not "is this a builtin".
# `emit` and `done` are the sharpest case: they are supply/react control-flow
# builtins, so a call that fails to resolve to the lexical does not just
# return the wrong value -- it hijacks the enclosing routine's control flow.
# See todo/tickets/code-lexical-does-not-shadow-a-builtin.md.

plan 15;

# `my &emit` at an outer scope, called from a nested sub.
{
    my &emit = { 'e' };
    sub emit-outer-lexical() {
        my @out = 1;
        @out.push(emit());
        @out;
    }
    is-deeply emit-outer-lexical(), [1, 'e'], 'my &emit shadows the emit builtin';
}

# A positional Callable parameter.
sub emit-positional(&emit) { emit() }
is emit-positional({ 'e' }), 'e', 'a positional &emit parameter shadows the builtin';

# A named Callable parameter -- the exact shape roast's Test::Tap uses.
sub emit-named(:&emit) { emit() }
is emit-named(emit => { 'e' }), 'e', 'a named :&emit parameter shadows the builtin';

# The explicitly sigiled call form is the same binding.
{
    my &emit = { 'e' };
    sub emit-sigiled() {
        my @out = 1;
        @out.push(&emit());
        @out;
    }
    is-deeply emit-sigiled(), [1, 'e'], '&emit() (explicit sigil) shadows the builtin too';
}

# `done` is the harder case: a bare `done()` statement used to be rewritten to
# the react/supply completion control-flow unconditionally, ignoring any
# lexical in scope.
sub done-positional(&done) { done() }
is done-positional({ 'd' }), 'd', 'a positional &done parameter shadows the builtin';

sub done-named(:&done) { done() }
is done-named(done => { 'd' }), 'd', 'a named :&done parameter shadows the builtin';

{
    my &done = { 'd' };
    is done(), 'd', 'my &done shadows the done builtin (call form)';
}

{
    my &done = { 'd' };
    my $r = False ?? 'x' !! done;
    is $r, 'd', 'my &done shadows the bareword `done` term too';
}

# The exact Test::Tap::tap-ok idiom: a nested closure captures the enclosing
# sub's `:&emit`/`:&done` parameter and calls it conditionally with `if`.
sub tap-emit-like(:&emit) {
    my @res;
    my $tap = { emit() if &emit; @res.push($_) };
    $tap(42);
    @res;
}
is-deeply tap-emit-like(), [42], 'emit() if &emit with no &emit passed just skips it';
{
    my @emitted;
    is-deeply tap-emit-like(emit => { @emitted.push('emitted') }), [42],
      'emit() if &emit calls the lexical, not the builtin, from a nested closure';
    is-deeply @emitted, ['emitted'], 'the lexical &emit was actually invoked';
}

sub tap-done-like(:&done) {
    my $called;
    my $tap = { done() if &done; $called = True };
    $tap();
    $called;
}
ok tap-done-like(), 'done() if &done with no &done passed just runs the rest';
{
    my $seen;
    sub tap-done-like2(:&done) {
        my $tap = { done() if &done; 'ran' };
        $tap();
    }
    is tap-done-like2(done => { $seen = True }), 'ran',
      'done() if &done calls the lexical, not the builtin, from a nested closure';
    ok $seen, 'the lexical &done was actually invoked';
}

# A non-final statement call (`name(); other-statement`) must resolve the
# same lexical binding as the last-statement / expression-position form --
# the statement-level compiler path used to skip the local-binding check
# entirely and fall straight to name-based builtin dispatch.
{
    my &emit = { 'e' };
    my $blk = { emit(); 'after' };
    is $blk(), 'after', 'a non-final emit() statement still resolves the lexical &emit';
}

# vim: expandtab shiftwidth=4
