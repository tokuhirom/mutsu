use Test;

# `done` used as a bare term in expression position (e.g. the `!!` branch of a
# ternary inside a `whenever`) must signal supply/react completion, exactly like
# the statement form. Previously it parsed to a no-op bareword, so an on-demand
# supply never completed and `await` spun to the poll deadline (~30s).

plan 4;

# `done` in the else branch of a ternary completes the supply promptly.
{
    my $start = now;
    await supply {
        whenever Supply.interval(0.01) {
            False ?? emit(1) !! done
        }
    };
    ok now - $start < 5, 'await of supply whose whenever runs `done` (else branch) returns promptly';
}

# emit-then-done returns the last emitted value.
{
    is (await supply { whenever Supply.interval(0.01) { emit 42; done } }), 42,
        'emit then done returns the emitted value';
}

# A whenever that dies (then branch) breaks the awaited promise promptly.
{
    my $start = now;
    my $threw = False;
    try {
        await supply { whenever Supply.interval(0.01) { True ?? die('boom') !! done } };
        CATCH { default { $threw = True } }
    }
    ok $threw && now - $start < 5, 'a dying whenever breaks the await promptly';
}

# A mix of done and die across many parallel supplies completes promptly.
{
    my $start = now;
    my @proms = (1..40).map: -> $i {
        start {
            await supply {
                whenever Supply.interval(0.001) {
                    $i > 20 ?? die 'boom' !! done
                }
            }
        }
    };
    my $threw = False;
    try { await @proms; CATCH { default { $threw = True } } }
    ok $threw && now - $start < 10,
        'parallel supplies that done/die complete promptly without spinning';
}
