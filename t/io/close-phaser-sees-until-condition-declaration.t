use Test;
plan 5;

# `until my $done { ... }` declares `$done` in the enclosing block, so a CLOSE
# phaser later in that block names the same variable and can end the loop.
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
    is @received, [1, 2, 3, 4, 5], 'values reach the consumer';
    ok $ran-close, 'the CLOSE phaser ran';
    is @pre-emit.elems, 5, 'the CLOSE phaser stopped the loop: no emit past done';
}

# The loop-condition variable is visible after the loop, as in Raku.
{
    my $n = 0;
    until my $stop { $n++; $stop = True if $n >= 3 }
    is $n, 3, 'the loop ran to the assigned stop';
    ok $stop, 'the declared variable is visible after the loop';
}
