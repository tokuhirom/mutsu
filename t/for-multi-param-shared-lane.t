use Test;

plan 4;

# A multi-parameter `for` loop binds a FRESH per-iteration lexical. It must not
# publish that binding into the cross-thread bare-name store, where an unrelated
# frame that happens to use the same variable name would read it back at the
# next `await` (`sync_shared_vars_to_env`).

sub compose(@components) {
    my $last;
    for @components.kv -> $i, $comp {
        $last = $i + $comp;
    }
    $last
}

# Activate the cross-thread store.
await start { 1 };

{
    my @seen;
    for 1..5 -> $i {
        compose([10, 20, 30, 40, 50]);
        await start { 1 };
        @seen.push($i);
    }
    is @seen.join(','), '1,2,3,4,5',
        'an unrelated multi-param `for $i` does not rewrite this loop\'s $i';
}

# The same collision through a `while` loop's own re-declared name is already
# masked; check the multi-param loop does not resurrect it via a hash source.
sub walk(%h) {
    my $n = 0;
    for %h.kv -> $k, $v {
        $n = $v;
    }
    $n
}

{
    my @seen;
    for <a b c> -> $k {
        walk({ x => 1, y => 2 });
        await start { 1 };
        @seen.push($k);
    }
    is @seen.join(','), 'a,b,c',
        'a multi-param `for $k, $v` does not rewrite an unrelated $k';
}

# Guard against over-masking: a `start` block that genuinely captures a
# multi-param loop variable must still observe it (capture-by-cell, not the
# name lane).
{
    my @got = await do for (10, 20).kv -> $i, $v {
        start { $i * 100 + $v }
    };
    is @got.join(','), '10,120',
        'a `start` capturing multi-param loop variables still sees them';
}

# ... and a nested multi-param loop of the same names still nests correctly.
{
    my @trace;
    for (1, 2).kv -> $i, $v {
        for (7, 8).kv -> $i, $v {
            @trace.push("in:$i/$v");
        }
        @trace.push("out:$i/$v");
    }
    is @trace.join(' '), 'in:0/7 in:1/8 out:0/1 in:0/7 in:1/8 out:1/2',
        'nested multi-param loops reusing the same names keep their bindings';
}
