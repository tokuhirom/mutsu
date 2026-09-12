use Test;

# ADR-0092 / issue #7565: a closure's captured env is installed as a TIER below
# the frame's whole parent chain (`overlay -> caller chain -> GLOBAL_BASE ->
# capture`) instead of being merged into the frame overlay one key at a time.
#
# The merge it replaces was `entry_or_insert_sym_with` — don't overwrite
# anything already visible from the caller — with an explicit overwrite list on
# top. These tests pin the resulting precedence, which is the whole observable
# surface of the change: what the capture provides, what beats it, and who else
# can see it.

plan 9;

# 1. The plain case: a captured lexical the caller knows nothing about still
#    resolves in the closure body. This is the fallback tier doing its job.
{
    my $secret = 'captured';
    my &peek = sub { $secret };
    sub call-it(&c) { c() }
    is call-it(&peek), 'captured', 'a captured lexical resolves through the fallback tier';
}

# 2. A dynamic variable is dynamic-scope: the LIVE caller binding wins over the
#    one the closure captured. This is the don't-overwrite default — the whole
#    reason the capture is a tier *below* the caller chain rather than seeded
#    into the frame overlay above it.
{
    my $*mode = 'creator';
    my &report = sub { $*mode };
    sub with-mode(&c) {
        my $*mode = 'caller';
        c();
    }
    is with-mode(&report), 'caller', 'a dynamic resolves to the live caller binding, not the capture';
    is report(), 'creator', 'and to the creating scope when the caller sets none';
}

# 3. A routine called FROM the closure body still resolves captured names
#    through the chain. The merge used to leave them in the frame overlay,
#    which a callee sees through its parent chain; the fallback tier is
#    consulted on the way back up for exactly this reason.
{
    my $*inherited = 'from-capture';
    my &outer = sub { inner() };
    sub inner() { $*inherited }
    sub launder(&c) { c() }
    is launder(&outer), 'from-capture', 'a callee of the closure still sees a captured name';
}

# 4. A closure created INSIDE a closure body must inherit the outer capture —
#    the capture walk has to start at the fallback tier or the inner closure is
#    built from an env that is missing every name the outer one captured.
{
    my $deep = 'outer-capture';
    my &make = sub { sub { $deep } };
    sub run-twice(&c) { my &got = c(); got() }
    is run-twice(&make), 'outer-capture', 'a closure built inside a closure keeps the outer capture';
}

# 5. A write inside the closure body shadows the captured value in the frame
#    overlay, which is above the fallback tier, and does not corrupt the
#    capture for the next call.
{
    my $*counter = 0;
    my &bump = sub { my $*counter = $*counter + 1; $*counter };
    is bump(), 1, 'a write inside the body shadows the captured value';
    is bump(), 1, 'and leaves the capture itself untouched for the next call';
}

# 6. `EVAL` resolves against the live env chain rather than its own lexical
#    scope, so it is the consumer most likely to notice where captured names
#    live. It must still find them.
{
    use MONKEY-SEE-NO-EVAL;
    my $evaluated = 'visible-to-eval';
    my &peek = sub { EVAL '$evaluated' };
    sub indirect(&c) { c() }
    is indirect(&peek), 'visible-to-eval', 'EVAL in the closure body sees a captured name';
}

# 7. Two closures from one factory each keep their own capture. The frame env's
#    capture tier is part of its identity (`tier_addrs`/`tier_maps`), so the
#    capture memo cannot hand the second closure the first one's entries.
{
    sub factory($tag) { sub { $tag } }
    my &a = factory('first');
    my &b = factory('second');
    is (a(), b()).join(','), 'first,second', 'sibling closures keep distinct captures';
}
