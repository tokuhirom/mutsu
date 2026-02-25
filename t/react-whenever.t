use Test;
plan 2;

# Test 1: whenever reacts to emits via Supply.on-demand
{
    my @values;
    my $s = Supply.on-demand(-> $e {
        $e.emit(2);
        $e.emit(3);
        $e.done;
    });
    react {
        whenever $s -> $x { @values.push($x); }
    }
    is @values.elems, 2, 'whenever reacts to on-demand supply';
}

# Test 2: whenever sees existing values from on-demand supply
{
    my @seen;
    my $s2 = Supply.on-demand(-> $e {
        $e.emit(1);
        $e.done;
    });
    react {
        whenever $s2 -> $x { @seen.push($x); }
    }
    is @seen.elems, 1, 'whenever sees values from on-demand supply';
}
