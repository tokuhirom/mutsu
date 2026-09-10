use Test;

# `cas` WRITES its target, but the compiler recorded it with
# `counts_as_write = false`, so the name never reached `self_mutated` and a
# lexical captured by a closure created BEFORE the `cas` was never promoted to
# a shared cell. The closure kept its by-value capture and never saw the swap:
#
#   my $x = 1; my $o = { $x }; cas $x, $x, 5; $o()   # raku: 5   mutsu: 1
#
# The direct read was always right, so only the closure lane diverged. Every
# other atomic op (`⚛++`, `⚛=`, `atomic-assign`, ...) already counted as a
# write and was already coherent — this brings `cas` in line with them.

plan 12;

# --- the ticket's own repro: an Instance-valued scalar --------------------
{
    my class Node { has $.k }
    my $head = Node.new(k => 1);
    my $obs  = { $head.k };
    cas $head, $head, Node.new(k => 2);
    is $obs(), 2, 'a closure captured before the cas sees the swapped Instance';
    is $head.k, 2, 'and the direct read still agrees';
}
# A type constraint changes nothing.
{
    my class Node { has $.k }
    my Node $head = Node.new(k => 1);
    my $obs = { $head.k };
    cas $head, $head, Node.new(k => 7);
    is $obs(), 7, 'the same holds for a typed scalar';
}

# --- the same lane fork for a plain Int ----------------------------------
{
    my $x = 1;
    my $o = { $x };
    cas $x, $x, 5;
    is $o(), 5, 'a closure sees a cas on an Int-valued scalar';
    is $x, 5, 'and the direct read agrees';
}

# --- a failed cas must not publish anything ------------------------------
{
    my $x = 1;
    my $o = { $x };
    cas $x, 99, 5;
    is $o(), 1, 'a cas whose expected value does not match leaves the closure alone';
    is $x, 1, 'and leaves the variable alone';
}

# --- repeated swaps stay coherent ----------------------------------------
{
    my $x = 0;
    my $o = { $x };
    cas $x, 0, 1;
    is $o(), 1, 'first swap is visible';
    cas $x, 1, 2;
    is $o(), 2, 'second swap is visible';
    $x = 9;
    is $o(), 9, 'and a plain assignment after the cas still is';
}

# --- non-regression: the other atomic ops were already coherent ----------
{
    my atomicint $n = 1;
    my $o = { $n };
    $n⚛++;
    is $o(), 2, 'an atomic increment is still visible to the closure';
    $n ⚛= 40;
    is $o(), 40, 'an atomic assign is still visible too';
}
