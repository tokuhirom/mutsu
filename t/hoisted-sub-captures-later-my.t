use Test;

plan 6;

# A hoisted sub that closes over a `my $x` declared *after* it must keep every
# write, even when it is called before the declaration statement runs at
# runtime. The declaration is compile-time, so the lexical container exists;
# previously the first call's mutation was dropped (`0,0,1` instead of `0,1,2`).
# Regression for roast S02-names-vars/variables-and-packages.t tests 24-25/27-28.

{
    is foo(), 0, "first call to hoisted sub (var not yet declared)";
    is foo(), 1, "second call keeps the first write";
    is foo(), 2, "third call accumulates";

    my $a;
    sub foo { $a++ }
}

# Same, but the `my` carries a runtime initializer that has not executed yet at
# call time — the calls still see the uninitialized (0) value and accumulate.
{
    is bar(), 0, "runtime part of `my` not yet executed (1)";
    is bar(), 1, "runtime part of `my` not yet executed (2)";
    is bar(), 2, "runtime part of `my` not yet executed (3)";

    my $a = 3;
    sub bar { $a++ }
}
