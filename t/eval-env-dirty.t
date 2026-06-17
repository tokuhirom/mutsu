use Test;

# Slice B (docs/vm-single-store.md): the EVAL / interpreter-fallback carrier
# reconciles exactly the caller lexicals it wrote back into their slots at the
# carrier-return boundary (replacing the blanket env_dirty reverse pull). These
# cases pin that every shape of caller-lexical write through the carrier still
# lands in the caller's slot.

plan 12;

# Scalar write by EVAL into a caller lexical.
{
    my $x = 1;
    EVAL '$x = 41';
    is $x, 41, 'EVAL scalar write reconciles into the caller slot';
}

# Read-modify-write referencing the caller value.
{
    my $n = 10;
    EVAL '$n = $n * 2 + 1';
    is $n, 21, 'EVAL read-modify-write sees and updates the caller slot';
}

# Repeated EVAL in a loop (the per-iteration reconcile must accumulate).
{
    my $c = 0;
    EVAL '$c = $c + 1' for ^100;
    is $c, 100, 'EVAL in a loop accumulates correctly';
}

# Array write by EVAL.
{
    my @a = 1, 2, 3;
    EVAL '@a.push(4)';
    is @a.join(','), '1,2,3,4', 'EVAL array push reconciles';
}

# Whole-array reassignment by EVAL.
{
    my @a = 1, 2;
    EVAL '@a = 7, 8, 9';
    is @a.join(','), '7,8,9', 'EVAL whole-array reassignment reconciles';
}

# Hash write by EVAL.
{
    my %h = a => 1;
    EVAL '%h<b> = 2';
    is %h<b>, 2, 'EVAL hash element write reconciles';
}

# Multiple caller lexicals written in one EVAL.
{
    my $p = 0;
    my $q = 0;
    EVAL '$p = 3; $q = 4';
    is "$p,$q", '3,4', 'EVAL writes to multiple caller lexicals';
}

# A caller lexical NOT written by EVAL must keep its (possibly freshly-set) value.
{
    my $kept = 99;
    my $touched = 0;
    EVAL '$touched = 1';
    is $kept, 99, 'untouched caller lexical is preserved across EVAL';
    is $touched, 1, 'touched caller lexical is updated';
}

# Nested EVAL writing an outer lexical.
{
    my $z = 0;
    EVAL 'EVAL q[$z = 5]';
    is $z, 5, 'nested EVAL reconciles the outer lexical';
}

# EVAL that throws must not corrupt caller state (and must be catchable).
{
    my $before = 7;
    my $caught = 0;
    try { EVAL 'die "boom"'; CATCH { default { $caught = 1 } } }
    is $caught, 1, 'EVAL that dies is catchable';
    is $before, 7, 'caller lexical intact after a dying EVAL';
}
