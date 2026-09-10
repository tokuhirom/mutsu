use v6;
use Test;

# ADR-0078: the five per-call scope stacks (`block_declared_vars`,
# `loop_local_vars`, `loop_local_saved_env`, `active_loop_param_names`,
# `active_loop_rw_param_names`) used to be isolated from a callee by
# `std::mem::take` -- the callee got a fresh empty vector and the caller's was
# moved into a Rust local. They now share one vector per field, and a call is a
# base index into it.
#
# Two things that used to be free must now be enforced explicitly, and both are
# silent when wrong:
#   * a callee must see NO frames of its caller (the `Deref` window is
#     `[base ..]`), and
#   * a callee that pops one frame too many must not eat its CALLER's frame
#     (`pop` is floored at the base; it used to be harmless because the callee's
#     vector was empty).
# Every check below is a construct whose correct answer depends on one of those.

plan 10;

# --- block-scope declarations must not register in the caller's block ---------
# `my $r` at the callee's routine level runs before the callee enters any block
# of its own. Without isolation it registers in the CALLER's active BlockScope
# frame and is reverted to its pre-block value at the caller's block exit.
sub block-recur($n) {
    my $r = 0;
    { $r = 10; block-recur($n - 1) if $n > 0 }
    $r
}
is block-recur(0), 10, 'a block-scoped write survives with no recursion';
is block-recur(4), 10, "a callee's own `my` does not register in the caller's block scope";

# Nested blocks in the caller, a call from the innermost one.
sub deep-blocks($n) {
    my $out = 0;
    { my $a = 1; { my $b = 2; { $out = $a + $b + deep-blocks($n - 1) if $n > 0 } } }
    $out
}
is deep-blocks(3), 9, 'nested caller blocks survive a call from the innermost one';

# --- loop-body-local declarations must not register in the caller's loop ------
# The `while` twin of the above: the callee's `my` landing in the caller's
# active loop-local scope made it clobbered at the caller's loop exit.
sub loop-recur($n) {
    my $r = 0;
    my @w = (1,);
    while @w.splice { loop-recur($n - 1) if $n > 0; $r += 10 }
    $r
}
is loop-recur(0), 10, 'a while-body write survives with no recursion';
is loop-recur(4), 10, "a callee's own `my` does not register in the caller's loop scope";

sub for-recur($n) {
    my $r = 0;
    for ^2 { for-recur($n - 1) if $n > 0; $r += 100 }
    $r
}
is for-recur(3), 200, 'a for-body accumulator survives calls made from the body';

# --- loop parameters: the callee must not inherit the caller's ---------------
# `active_loop_param_names` decides whether a closure's free variable is an
# outer loop's per-iteration binding. A callee's own free variable that merely
# shares an enclosing loop's parameter name must not be mistaken for it.
sub make-closure() {
    my $x = 'callee';
    -> { $x }
}
my @closures;
for <a b> -> $x {
    @closures.push(make-closure());
}
is @closures.map({ .() }).join(','), 'callee,callee',
    "a callee's closure does not capture the caller loop's parameter binding";

# Per-iteration capture inside one frame still works (the caller's own frames
# are visible to the caller, just not to its callees).
my @per-iter;
for <p q r> -> $y {
    @per-iter.push(-> { $y });
}
is @per-iter.map({ .() }).join(','), 'p,q,r',
    'per-iteration loop-parameter capture is unaffected';

# --- a shadowing loop-local `my` restores the outer binding at loop exit ------
# This is `loop_local_saved_env`: the pre-loop value lives in a scope frame for
# the duration of the loop body, and must survive both a deep call made from
# inside the body (it is a GC root only through that frame) and the loop's exit.
# `burn` allocates a fresh object per level so the descent puts the collector to
# work while the shadow's saved frame is suspended.
class Node { has $.v is rw }
sub burn($n) {
    my $held = Node.new(v => $n);
    burn($n - 1) if $n > 0;
    $held.v
}
my $shadowed = Node.new(v => 7);
my $inner-seen;
for ^2 {
    my $shadowed = Node.new(v => 1);
    burn(40);
    $inner-seen = $shadowed.v;
}
is $inner-seen, 1, 'the loop-local shadow is what the body sees';
is $shadowed.v, 7,
    'the shadowed outer binding survives a deep call from the loop body and the loop exit';
