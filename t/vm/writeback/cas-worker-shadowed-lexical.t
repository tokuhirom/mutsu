use Test;

plan 6;

# A worker-local `my` is a different binding from the same-named outer
# lexical. CAS must use that binding's cell rather than the legacy lane keyed
# only by the bare name, or await's shared-variable reconciliation writes the
# worker result back over the outer lexical.
{
    my $outer = 1;
    my $worker = start {
        my $outer = 100;
        cas $outer, -> $value { $value + 1 };
        $outer;
    };

    is $worker.result, 101, 'CAS updates the worker-local lexical';
    is $outer, 1, 'CAS does not leak a shadowed worker lexical to the outer binding';
}

# The three-argument form takes the same binding-identity path.
{
    my $outer = 1;
    my $worker = start {
        my $outer = 100;
        is cas($outer, 100, 101), 100, 'three-argument CAS sees the worker-local value';
        $outer;
    };

    is $worker.result, 101, 'three-argument CAS updates the worker-local lexical';
    is $outer, 1, 'three-argument CAS does not leak to the outer binding';
}

# Atomic cell bindings remain intentionally shared across start blocks.
{
    my atomicint $counter = 0;
    await (^4).map: { start { $counter⚛++ } };
    is $counter, 4, 'atomicint counter remains shared across workers';
}
