use Test;

# Coherence pin for the env<->locals single-store path
# (MUTSU_NO_BLANKET_RECONCILE): `undefine($x) = v` is rw — it writes `v` back
# to the target var. Like substr-rw it updated `env[$x]` but not the caller's
# local slot, so under the single-store path the slot kept the original value.
# Must pass identically with and without the blanket reconcile.

plan 3;

{
    my $a = "foo";
    undefine($a) = "bar";
    is $a, "bar", "undefine lvalue assign reaches the local slot";
}

{
    my $n = 41;
    undefine($n) = 99;
    is $n, 99, "undefine lvalue assign on a numeric var reaches the slot";
}

{
    my $a = "x";
    undefine($a) = "1";
    undefine($a) = "2";
    is $a, "2", "successive undefine lvalue assigns land on the slot";
}
