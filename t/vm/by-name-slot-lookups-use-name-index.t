use Test;

plan 10;

# The by-name local-slot lookups behind `SetVarDynamic`'s `state` test,
# `OUTER::`, a paired-less `MarkSigillessBind`, the `our`-alias sync of a
# symbolic store and `do given` go through the chunk's name index instead of
# scanning every local (#9171). These pin the answers those lookups give.

# `state` vs `my` inside a loop body once the cross-thread shared store is
# active (the `is_state` test in SetVarDynamic only runs then).
await start { 1 };
my @seen;
for ^3 {
    state $n = 0;
    my $m = 0;
    $n++;
    $m++;
    @seen.push: "$n/$m";
}
is @seen.join(','), '1/1,2/1,3/1', 'state persists and my resets with threads active';

sub counter { state $c = 0; my $d = 10; $c++; $d++; "$c/$d" }
counter() for ^2;
is counter(), '3/11', 'a sub keeps its state var across calls with threads active';

# OUTER:: reads the binding one scope out, not the outermost same-named one.
my $a = 'outer';
{
    my $a = 'middle';
    {
        my $a = 'inner';
        is $OUTER::a, 'middle', 'OUTER:: resolves to the next scope out';
        is $OUTER::OUTER::a, 'outer', 'OUTER::OUTER:: resolves two scopes out';
    }
}

# A sigilless binding's writability follows what it was bound to.
my \ro = 5;
throws-like { ro = 6 }, X::Assignment::RO, 'a sigilless term bound to a value is read-only';
my $cell = 1;
my \rw = $cell;
rw = 7;
is $cell, 7, 'a sigilless term bound to a container writes through';

# A symbolic store to a package variable reaches its `our` alias.
package SymPkg {
    our $v = 1;
    $::('SymPkg::v') = 5;
    is $v, 5, 'a symbolic store updates the our-linked lexical alias';
}
is $SymPkg::v, 5, 'and the package variable itself';

# `do given` keeps a lexical `$_` slot in step with its topic.
my $r = do given 42 { $_ + 1 };
is $r, 43, 'do given sees its topic';

# A `my` redeclared many times in one sub stays independent per name.
sub many {
    my $x0 = 0; my $x1 = 1; my $x2 = 2; my $x3 = 3; my $x4 = 4;
    my $x5 = 5; my $x6 = 6; my $x7 = 7; my $x8 = 8; my $x9 = 9;
    $x0 + $x1 + $x2 + $x3 + $x4 + $x5 + $x6 + $x7 + $x8 + $x9
}
is (many() for ^3).sum, 135, 'a sub with many my declarations';
