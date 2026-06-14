use v6;
use Test;

# A `react`/`whenever` consuming an on-demand `supply { ... }` whose body emits
# synchronously must stream values to the consumer and terminate the body when
# the consumer signals `done` (emit-to-dead-consumer). See S17-supply/syntax.t.

plan 8;

# Infinite `loop { emit }` body: terminates one emit past the consumer's done.
{
    my $pre-emits = 0;
    my $post-emits = 0;
    sub make-supply() {
        supply {
            loop {
                $pre-emits++;
                emit(++$);
                $post-emits++;
            }
        }
    }

    my @received;
    react {
        whenever make-supply() -> $n {
            push @received, $n;
            done if $n >= 5;
        }
    }

    is @received, [1, 2, 3, 4, 5], 'infinite synchronous supply streams values';
    is $pre-emits, 6, 'body runs one emit past the dead consumer';
    is $post-emits, 5, 'body terminates on emit to dead consumer';
}

# `until my $done { ... } CLOSE { $done = True }`: the CLOSE phaser (registered
# at block entry, even though it textually follows the loop) fires when the
# consumer signals `done`, setting $done so the loop exits without an extra emit.
{
    my @pre-emit;
    my $ran-close = False;
    sub make-supply() {
        supply {
            until my $done {
                @pre-emit.push('x');
                emit(++$);
            }
            CLOSE { $ran-close = True; $done = True }
        }
    }

    my @received;
    react {
        whenever make-supply() -> $n {
            push @received, $n;
            done if $n >= 5;
        }
    }

    is @received, [1, 2, 3, 4, 5], 'until-loop supply streams values';
    ok $ran-close, 'CLOSE phaser in source supply ran';
    is @pre-emit, ['x', 'x', 'x', 'x', 'x'], 'CLOSE breaks the loop, no extra emit';
}

# A finite synchronous supply still works through the streaming path.
{
    my @received;
    react {
        whenever supply { emit 10; emit 20; emit 30 } -> $n {
            push @received, $n;
        }
    }
    is @received, [10, 20, 30], 'finite synchronous supply delivers all values';
}

# Consumer that never signals done drains a finite supply fully.
{
    my $sum = 0;
    react {
        whenever supply { emit $_ for 1..4 } -> $n { $sum += $n }
    }
    is $sum, 10, 'finite supply with for-emit sums correctly';
}
