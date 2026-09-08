use Test;

# The caller-var writeback drain walks this frame's locals and asks the pending
# set about each, instead of asking `find_local_slot` -- a linear `code.locals`
# scan -- about each pending source. Same answer, O(locals) hash lookups instead
# of O(pending * locals) string comparisons.
#
# The rewrite matters because the pending set is long-lived and large:
# retain-on-miss never removes a name no frame will ever own, and
# `merge_method_env` feeds it every caller-visible env key a method changed --
# so while a module loads, its enum values, constants and exported symbols. The
# Cro HTTP/2 header path left 313 such permanently unclaimable names, and
# rescanning them on every call return was 8% of all instructions executed
# (#7667).
#
# This file pins that the rewrite still delivers every writeback a real frame
# can claim, across the frame shapes that feed the retain-on-miss list. Dropping
# an unclaimed source instead is NOT sound -- see the proxy-bind case in
# t/substr-rw-lvalue-writeback-coherence.t, which is recorded while a `code`
# without the slot is current and claimed by a later drain in the same frame.

plan 10;

# --- retained across an intervening deeper call, then claimed at the mainline ---
{
    sub deeper() { 99 }
    sub writer() {
        callframe(1).my.<$a> = 7;
        deeper();               # drains at depth 1: must NOT drop
    }
    my $a is dynamic = 0;
    writer();
    is $a, 7, 'caller-frame write survives an intervening deeper call';
}

# --- $CALLER:: read-modify-write from the mainline ---
{
    my sub bump { $CALLER::b++ }
    my $b is dynamic = 41;
    bump();
    is $b, 42, '$CALLER::b++ reaches the mainline slot';
}

# --- claimed two frames up, with the miss happening in between ---
{
    sub inner() { callframe(2).my.<$c> = 5 }
    sub middle() { inner() }    # drains at depth 1 and misses: must retain
    my $c is dynamic = 0;
    middle();
    is $c, 5, 'a write targeting the grandparent frame survives the miss between';
}

# --- sibling BUILD writes are not consumed by a nested dispatch ---
{
    my $parent-builds = 0;
    class S { }
    class P { submethod BUILD { $parent-builds++ } }
    class C is P { submethod BUILD { my $x = S.new } }
    C.new;
    is $parent-builds, 1, 'nested .new in child BUILD keeps the parent BUILD write';
}

# --- captured-outer mutation from a method, claimed at the mainline ---
{
    my $total = 0;
    class Acc { method add($n) { $total += $n } }
    my $acc = Acc.new;
    $acc.add(3);
    $acc.add(4);
    is $total, 7, 'a method mutating a captured mainline lexical accumulates';
}

# --- the same, with a nested call inside the method ---
{
    my $seen = 0;
    class Nest {
        method helper() { 1 }
        method go() { $seen += self.helper() }
    }
    my $n = Nest.new;
    $n.go();
    $n.go();
    is $seen, 2, 'captured-outer write survives a nested call inside the method';
}

# --- is rw parameter writeback from the mainline ---
{
    sub setit($x is rw) { $x = 11 }
    my $r = 0;
    setit($r);
    is $r, 11, 'is rw parameter writes back to the mainline slot';
}

# --- is rw writeback with a deeper call before the callee returns ---
{
    sub noise() { 1 }
    sub setit2($x is rw) { $x = 12; noise() }
    my $r2 = 0;
    setit2($r2);
    is $r2, 12, 'is rw writeback survives a deeper call inside the callee';
}

# --- a closure mutating a mainline lexical, called repeatedly ---
{
    my $count = 0;
    my $inc = { $count++ };
    $inc() for ^3;
    is $count, 3, 'closure mutating a mainline lexical accumulates across calls';
}

# --- enum registration (the shape that filled the list) must not disturb a
#     live mainline lexical whose slot a later call refreshes ---
{
    my $keep = 0;
    enum Colour <RED GREEN BLUE>;
    class Painter { method paint() { $keep++ } }
    my $p = Painter.new;
    $p.paint();
    $p.paint();
    ok $keep == 2 && GREEN.value == 1,
        'enum registration leaves later caller-slot refreshes working';
}
