use v6;
use Test;

# A fresh `my $x = ...` at a NEW scope entry (e.g. a loop iteration) is a
# brand-new lexical, so it must not inherit a sigilless `:=` alias that a PRIOR
# iteration left. This showed up as a loop body doing `my $a = ...; $a := h`
# leaking the NEXT iteration's fresh `my $a = ...` back into the loop variable
# `h` (a sigilless alias target).
#
# This must NOT disturb a SAME-scope redeclaration, which in raku is the SAME
# variable (`my $x = 2; my $y := $x; my $x = 3` — `$y` still tracks `$x`), so
# only the forward alias for the redeclared name is dropped; reverse aliases and
# `local_bind_pairs` (which carry the same-scope binding) are preserved.

plan 4;

# The canonical leak: `$a := h` in iteration 1 must not make iteration 2's fresh
# `my $a = 777` write through to `h`.
{
    my @seen;
    for 10, 20 -> \h {
        my $a = 777;      # brand-new lexical each iteration; must not touch h
        @seen.push(h);
        $a := h;          # bind (leaves alias state for the next iteration)
    }
    is @seen, [10, 20], 'fresh `my $a` does not leak into a prior `:=`-bound loop var';
}

# Same with a scalar loop variable.
{
    my @seen;
    for <a b c> -> $x {
        my $t = 'X';
        @seen.push($x);
        $t := $x;
    }
    is @seen, [<a b c>], 'scalar loop var not clobbered by next iteration decl';
}

# A within-iteration `:=` still works (binding is genuine while in scope).
{
    my \h = 5;
    my $a := h;
    is $a, 5, '`:=` bind reads the bound value';
}

# A SAME-scope redeclaration is the SAME variable: a lexical bound to it must
# still see the redeclared value (raku: `my $y := $x; my $x = 3` => `$y == 3`).
{
    my $x = 2;
    my $y := $x;
    my $x = 3;
    is $y, 3, 'same-scope redeclaration keeps a `:=`-bound lexical tracking it';
}
