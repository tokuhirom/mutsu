use Test;
plan 7;

# ADR-0023: two concurrently-live `start {}` spawns of a `for LIST -> $x { }`
# loop must each keep their own per-iteration binding of `$x`, even when `$x`
# holds a non-"plain" value (an Instance) and even when some earlier,
# unrelated, fully-completed binding of the same bare name has already run
# (which is what previously poisoned the cross-thread bare-name lane).
# See docs/adr/0023-binding-provenance-spawn-capture.md and
# todo/deep/concurrent-for-loop-siblings-cannot-share-a-bare-loop-param-name.md
# (git mv'd to news/ once this lands).

class Widget {
    has $.id;
}

# 1. Warm-up variant: a `given EXPR -> $client { await ... }` that fully
# completes, using the SAME bare name as the later concurrent loop.
{
    given Widget.new(id => 'warmup') -> $client {
        await Promise.in(0.01);
    }
    my $client-a = Widget.new(id => 'A');
    my $client-b = Widget.new(id => 'B');
    my @promises = do for $client-a, $client-b -> $client {
        start {
            my @a;
            for 1..5 -> $i {
                await Promise.in(0.01);
                @a.push($client.id);
            }
            @a.join(',');
        }
    }
    is (await @promises).join(' | '), 'A,A,A,A,A | B,B,B,B,B',
        'given warm-up does not poison sibling for-loop spawns';
}

# 2. No warm-up at all.
{
    my $client-a = Widget.new(id => 'A');
    my $client-b = Widget.new(id => 'B');
    my @promises = do for $client-a, $client-b -> $client {
        start {
            my @a;
            for 1..5 -> $i {
                await Promise.in(0.01);
                @a.push($client.id);
            }
            @a.join(',');
        }
    }
    is (await @promises).join(' | '), 'A,A,A,A,A | B,B,B,B,B',
        'no warm-up: sibling for-loop spawns keep their own binding';
}

# 3. Warm-up with a DIFFERENT bare name — must not matter either way.
{
    given Widget.new(id => 'warmup') -> $warmup {
        await Promise.in(0.01);
    }
    my $client-a = Widget.new(id => 'A');
    my $client-b = Widget.new(id => 'B');
    my @promises = do for $client-a, $client-b -> $client {
        start {
            my @a;
            for 1..5 -> $i {
                await Promise.in(0.01);
                @a.push($client.id);
            }
            @a.join(',');
        }
    }
    is (await @promises).join(' | '), 'A,A,A,A,A | B,B,B,B,B',
        'renamed warm-up does not poison sibling for-loop spawns';
}

# 4. Plain block-scoped warm-up (not `given`), same bare name.
{
    { my $client = Widget.new(id => 'warmup'); await Promise.in(0.01); }
    my $client-a = Widget.new(id => 'A');
    my $client-b = Widget.new(id => 'B');
    my @promises = do for $client-a, $client-b -> $client {
        start {
            my @a;
            for 1..5 -> $i {
                await Promise.in(0.01);
                @a.push($client.id);
            }
            @a.join(',');
        }
    }
    is (await @promises).join(' | '), 'A,A,A,A,A | B,B,B,B,B',
        'plain-block warm-up does not poison sibling for-loop spawns';
}

# 5. Multi-param variant: `for LIST -> $x, $y { start {...} }`.
{
    my $a1 = Widget.new(id => 'A1');
    my $b1 = Widget.new(id => 'B1');
    my $a2 = Widget.new(id => 'A2');
    my $b2 = Widget.new(id => 'B2');
    my @promises;
    for $a1, $b1, $a2, $b2 -> $x, $y {
        @promises.push(start {
            my @a;
            for 1..5 -> $i {
                await Promise.in(0.01);
                @a.push($x.id ~ '/' ~ $y.id);
            }
            @a.join(',');
        });
    }
    is (await @promises).join(' | '),
        'A1/B1,A1/B1,A1/B1,A1/B1,A1/B1 | A2/B2,A2/B2,A2/B2,A2/B2,A2/B2',
        'multi-param for-loop spawns keep their own per-iteration bindings';
}

# 6. Channel-typed loop item, exercised cross-thread, with a same-named
# warm-up (the trigger condition from the deep-dive ticket).
{
    given "warmup" -> $c {
        await Promise.in(0.01);
    }
    my $chan-a = Channel.new;
    my $chan-b = Channel.new;
    my @results = do for $chan-a, $chan-b -> $c {
        start {
            await Promise.in(0.01);
            $c.WHICH.Str;
        }
    }
    my @actual = await @results;
    is-deeply [@actual[0] eq $chan-a.WHICH.Str, @actual[1] eq $chan-b.WHICH.Str],
        [True, True],
        'Channel-typed loop items keep their own identity across sibling spawns';
}

# 7. Multi-param variant through the `do for` EXPRESSION form specifically
# (not the statement form of #5) — the compiler path that builds ForLoopSpec
# from `Stmt::For` when it is used as an expression's tail value used to drop
# `multi_param_names`/`multi_param_locals` entirely
# (todo/tickets/do-for-expression-form-drops-multi-param-names.md), which
# defeated ADR-0023's masking for this specific shape even though the
# statement form (#5) already worked.
{
    my $a1 = Widget.new(id => 'A1');
    my $b1 = Widget.new(id => 'B1');
    my $a2 = Widget.new(id => 'A2');
    my $b2 = Widget.new(id => 'B2');
    my @promises = do for $a1, $b1, $a2, $b2 -> $x, $y {
        start {
            my @a;
            for 1..5 -> $i {
                await Promise.in(0.01);
                @a.push($x.id ~ '/' ~ $y.id);
            }
            @a.join(',');
        }
    }
    is (await @promises).join(' | '),
        'A1/B1,A1/B1,A1/B1,A1/B1,A1/B1 | A2/B2,A2/B2,A2/B2,A2/B2,A2/B2',
        'do-for expression-form multi-param spawns keep their own per-iteration bindings';
}
