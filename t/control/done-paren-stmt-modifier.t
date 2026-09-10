use Test;

# `done()` (explicit call form) followed by a statement modifier must parse as
# `(done()) if COND`, not `done; () if COND`. Previously the bare `done` was
# split off as an unconditional control statement and the trailing `()` became a
# separate term carrying the modifier, so `done() if $_ == N` fired on every
# value.

plan 4;

# Blockless react with `done() if` — should iterate until the condition holds.
{
    my $i = 0;
    react whenever Supply.from-list(0, 1, 2, 3, 4, 5) {
        done() if $_ == 3;
        $i++;
    }
    is $i, 3, 'done() if in blockless react iterates until the condition';
}

# Block form behaves the same.
{
    my $i = 0;
    react {
        whenever Supply.from-list(0, 1, 2, 3, 4, 5) {
            done() if $_ == 3;
            $i++;
        }
    }
    is $i, 3, 'done() if in block react iterates until the condition';
}

# `done()` with no modifier still completes immediately.
{
    my $i = 0;
    react {
        whenever Supply.from-list(10, 20, 30) {
            $i++;
            done();
        }
    }
    is $i, 1, 'bare done() completes the react after the first value';
}

# `done if` (no parens) keeps working.
{
    my $i = 0;
    react whenever Supply.from-list(0, 1, 2, 3, 4) {
        done if $_ == 2;
        $i++;
    }
    is $i, 2, 'done if (no parens) still parses the modifier correctly';
}
