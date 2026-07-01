use Test;

plan 5;

# A closure created in a loop body that captures a `:=`-bound loop-local
# (`my $x := @a[$i]`) read-only must freeze that iteration's binding, so each
# closure reads its own element — not the loop's final binding.

# Read a single `:=`-bound capture.
{
    my @in = 1..5;
    my @cb;
    for ^5 -> $i {
        my $in := @in[$i];
        @cb.push: { $in * 10 };
    }
    my @r = @cb.map({ $_() });
    is-deeply @r, [10, 20, 30, 40, 50], 'read-only := capture is per-iteration';
}

# Two `:=`-bound captures, one read + one write.
{
    my @in = 1..5;
    my @out = 0 xx 5;
    my @cb;
    for ^5 -> $i {
        my $in  := @in[$i];
        my $out := @out[$i];
        @cb.push: { $out = $in * 10 };
    }
    $_() for @cb;
    is-deeply @out, [10, 20, 30, 40, 50], 'read+write := captures are per-iteration';
}

# C-style loop with `:=` bindings.
{
    my @in = 1..4;
    my @out = 0 xx 4;
    my @cb;
    loop (my $i = 0; $i < @in; $i++) {
        my $in  := @in[$i];
        my $out := @out[$i];
        @cb.push: { $out = $in + 100 };
    }
    $_() for @cb;
    is-deeply @out, [101, 102, 103, 104], 'C-style loop := captures are per-iteration';
}

# A regular `my $x = ...` capture (not `:=`) still works.
{
    my @cb;
    for ^3 -> $i {
        my $x = $i * 2;
        @cb.push: { $x };
    }
    my @r = @cb.map({ $_() });
    is-deeply @r, [0, 2, 4], 'plain my capture is per-iteration';
}

# A mutated capture keeps a live shared cell (accumulator), not a value freeze.
{
    my $sum = 0;
    my @cb;
    for ^3 {
        @cb.push: { $sum++ };
    }
    $_() for @cb;
    is $sum, 3, 'mutated capture stays a live shared cell';
}
