use v6;
use Test;

# Pin for the §1.3 lexical-slot endgame slice: under shadow slots, block exit is
# slot-authoritative — a propagating enclosing var keeps its live slot value
# instead of being re-seeded from the name-keyed env, while non-propagating names
# ($_, $*dyn, block-declared `my`) are still reverted. These cases exercise every
# arm of the restore so a regression in either direction is caught.

plan 11;

# 1. A block-body write to an ENCLOSING scalar propagates out of the block.
{
    my $x = 1;
    { $x = 2 }
    is $x, 2, 'enclosing scalar modified in a bare block propagates';
}

# 2. A block-declared `my` does NOT leak past the block.
{
    my $seen = 'outer';
    { my $seen = 'inner'; $seen ~= '!' }
    is $seen, 'outer', 'block-declared my does not leak to the outer scope';
}

# 3. A block-declared `my` that shadows an enclosing name leaves the outer intact.
{
    my $v = 10;
    { my $v = 99; $v++ }
    is $v, 10, 'shadowing my inside a block leaves the outer binding untouched';
}

# 4. An enclosing @-array mutated in a block propagates (container identity).
{
    my @a = 1, 2, 3;
    { @a.push(4) }
    is @a.join(','), '1,2,3,4', 'enclosing array push propagates out of the block';
}

# 5. An enclosing %-hash key set in a block propagates.
{
    my %h = a => 1;
    { %h<b> = 2 }
    is %h<b>, 2, 'enclosing hash key set in a block propagates';
}

# 6. The topic $_ is block-scoped: an inner given does not leak $_ outward.
{
    $_ = 'top';
    given 'inner' { }
    is $_, 'top', 'given restores the outer topic on block exit';
}

# 7. Re-entering a block (loop) does not carry a block-declared my across iterations.
{
    my @collected;
    for 1..3 -> $n {
        my $tmp = $n * 10;
        @collected.push($tmp);
    }
    is @collected.join(','), '10,20,30', 'block-declared my is fresh each loop iteration';
}

# 8. Nested blocks: innermost enclosing write propagates through both.
{
    my $d = 0;
    { { $d = 5 } }
    is $d, 5, 'enclosing write from a doubly-nested block propagates';
}

# 10. A dynamic variable is restored on block exit.
{
    my $*DYN = 'outer';
    { my $*DYN = 'inner'; is $*DYN, 'inner', 'inner dynamic var is visible in-block' }
    is $*DYN, 'outer', 'dynamic var restored to the outer value on block exit';
}

# 11. Assignment-through with an existing outer AND a computed value survives.
{
    my $sum = 0;
    for 1..4 { $sum += $_ }
    is $sum, 10, 'accumulator across a for-loop body propagates each iteration';
}
