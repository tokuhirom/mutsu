use Test;

# Slice A (docs/vm-single-store.md) is a measurement-only change to
# `sync_locals_from_env`: it restructures the env->locals pull to compare the
# old local against the new env value (under MUTSU_VM_STATS) before overwriting.
# These cases pin that the reverse-sync reconciliation itself is unchanged —
# every by-name writer class the measurement targets still writes through to the
# caller's slot correctly.

plan 5;

# EVAL writing a caller lexical (carrier / R2): the slot must observe the writes.
{
    my $x = 1;
    EVAL '$x = $x + 10';
    is $x, 11, 'EVAL write to a caller lexical reconciles into the slot';
}

# dynamic var write-through (R1) across a call boundary.
{
    my $*dyn = 0;
    sub bump() { $*dyn = $*dyn + 1 }
    bump() for ^5;
    is $*dyn, 5, 'dynamic var writes reconcile';
}

# our var (R1).
{
    our $counter = 0;
    $counter++ for ^7;
    is $counter, 7, 'our var increments reconcile';
}

# closure mutating an outer aggregate (R2 / upvalue).
{
    my @acc;
    my $adder = -> $v { @acc.push($v) };
    $adder($_) for ^4;
    is @acc.elems, 4, 'closure push to an outer array reconciles';
}

# method writing a local read back after the call (the bench-class `desc` shape).
{
    class C {
        has $.n;
        method label() { "n=" ~ $.n }
    }
    my $c = C.new(n => 42);
    is $c.label, 'n=42', 'method attribute read reconciles';
}
