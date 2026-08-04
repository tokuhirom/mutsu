use Test;

# ADR-0019 C6d-1: the `nextsame`/`callsame`/`nextwith`/`callwith` multi deferral
# runs the next candidate as bytecode (the body the declaration plan compiled)
# instead of recompiling that candidate's AST body on every deferral.
#
# The chain must keep owning its own multi-dispatch frame while doing so: an
# entry point that pushes a fresh frame for the name restarts the chain at the
# first candidate, so each `nextsame` defers to the same candidate forever. The
# deep chains below are what catch that -- they would recurse until the stack
# overflows rather than fail an assertion.

plan 18;

# A long deferral chain: every candidate matches, so the chain has to walk all
# the way to the end exactly once.
{
    my @order;
    proto sub deep($) {*}
    multi sub deep(Int $n where * > 0)   { @order.push: 'a'; nextsame }
    multi sub deep(Int $n where * > 1)   { @order.push: 'b'; nextsame }
    multi sub deep(Int $n where * > 2)   { @order.push: 'c'; nextsame }
    multi sub deep(Int $n where * > 3)   { @order.push: 'd'; nextsame }
    multi sub deep($n)                   { @order.push: 'z'; 'done' }
    is deep(9), 'done', 'a five-deep nextsame chain returns the last candidate value';
    is @order.elems, 5, 'each candidate in the chain ran exactly once';
    is @order.join(''), 'abcdz', 'the chain walked every candidate in order';
}

# Repeated calls must not accumulate state in the chain's frame.
{
    my @order;
    proto sub rep($) {*}
    multi sub rep(Int $n where * > 0) { @order.push: 'w'; nextsame }
    multi sub rep($n)                 { @order.push: 'g' }
    rep(1) for ^20;
    is @order.elems, 40, 'a repeated chain runs each candidate once per call';
    is @order.join('').substr(0, 4), 'wgwg', 'the per-call order is stable';
}

# `callsame` returns the deferred value rather than tail-calling.
{
    proto sub cs-chain($) {*}
    multi sub cs-chain(Int $n where * > 0) { 'saw:' ~ callsame() }
    multi sub cs-chain(Int $n where * > 1) { 'mid:' ~ callsame() }
    multi sub cs-chain($n)                 { 'base' }
    is cs-chain(5), 'saw:mid:base', 'callsame threads the value back up the chain';
}

# `nextwith` / `callwith` re-dispatch with new arguments.
{
    proto sub nw($) {*}
    multi sub nw(Int $n where * > 100) { nextwith(1) }
    multi sub nw(Int $n)               { "int $n" }
    is nw(500), 'int 1', 'nextwith re-dispatches with the replacement argument';

    proto sub cw($) {*}
    multi sub cw(Int $n where * > 100) { 'got ' ~ callwith(2) }
    multi sub cw(Int $n)               { "int $n" }
    is cw(500), 'got int 2', 'callwith re-dispatches and returns the value';
}

# A candidate that defers is itself recursive: the chain and the recursion must
# not be confused for one another.
{
    my @trace;
    proto sub rec($) {*}
    multi sub rec(Int $n where * > 0) {
        @trace.push: "w$n";
        rec($n - 1) if $n > 1;
        nextsame;
    }
    multi sub rec(Int $n) { @trace.push: "g$n"; $n }
    rec(3);
    is @trace.grep(* eq 'w3').elems, 1, 'the outer recursive candidate ran once';
    is @trace.grep(/^g/).elems, 3, 'each recursion level reached the generic candidate';
}

# An `is rw` parameter still writes back through a deferral.
{
    proto sub bump($ is rw) {*}
    multi sub bump(Int $n is rw where * >= 0) { $n = $n + 1; nextsame }
    multi sub bump($n is rw)                  { $n = $n * 10 }
    my $v = 4;
    bump($v);
    is $v, 50, 'the rw writeback survives the deferral chain';
}

# Named arguments and defaults bind in the deferred candidate.
{
    proto sub nmd($, :$tag) {*}
    multi sub nmd(Int $n where * > 0, :$tag) { 'pos-' ~ callsame() }
    multi sub nmd($n, :$tag = 'd')           { "n=$n tag=$tag" }
    is nmd(3, tag => 'x'), 'pos-n=3 tag=x',
        'a named argument reaches the deferred candidate';
    is nmd(3), 'pos-n=3 tag=d', 'the deferred candidate applies its own default';
}

# A `state` variable in a deferred candidate belongs to that candidate.
{
    proto sub st($) {*}
    multi sub st(Int $n where * > 0) { nextsame }
    multi sub st($n)                 { state $seen = 0; ++$seen }
    st(1); st(1);
    is st(1), 3, 'a deferred candidate keeps its own state across deferrals';
}

# Deferral inside a method chain (submethod/method candidates), to make sure the
# routine-side change did not disturb method re-dispatch.
{
    class Base { method m(Int $n) { "base $n" } }
    class Derived is Base { method m(Int $n) { 'derived ' ~ callsame() } }
    is Derived.new.m(2), 'derived base 2', 'method callsame still reaches the parent';
}

# `.candidates`-driven manual dispatch is unaffected by the chain change.
{
    proto sub cand($) {*}
    multi sub cand(Int $n) { "int $n" }
    multi sub cand(Str $s) { "str $s" }
    is &cand.candidates.elems, 2, 'both candidates are still visible';
    is cand(1), 'int 1', 'ordinary multi dispatch still selects by type';
    is cand('a'), 'str a', 'ordinary multi dispatch still selects the Str candidate';
}
