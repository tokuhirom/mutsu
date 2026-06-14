use v6;
use Test;

# `last` and `next` inside a `whenever` block in a `react`:
# - `next` skips the rest of the body for the current value and continues.
# - `last` stops just that `whenever` (firing its LAST phaser with the
#   triggering value as topic) without ending the whole react or throwing.
# See roast/S17-supply/syntax.t.

plan 8;

# next skips values
{
    my @got;
    react whenever Supply.from-list(1..6) {
        next if $_ %% 2;
        @got.push($_);
    }
    is-deeply @got, [1, 3, 5], 'next skips matching values in a whenever';
}

# next does not throw
{
    lives-ok {
        react whenever Supply.from-list(1..4) { next if $_ > 2 }
    }, 'next inside a whenever does not die';
}

# last stops the whenever and fires LAST with the triggering value as topic
{
    my @got;
    react whenever Supply.from-list(1..10) {
        last if $_ > 3;
        @got.push($_);
        LAST { @got.push("last $_") }
    }
    is-deeply @got, [1, 2, 3, "last 4"],
        'last stops the whenever and LAST runs with the triggering topic';
}

# last does not throw, even without a LAST block
{
    lives-ok {
        react whenever Supply.from-list(1..6) {
            last if $_ > 2;
            die "ran past last at $_" if $_ > 3;
        }
    }, 'last inside a whenever does not die (no LAST block)';
}

# last on a single whenever sees all values up to (but excluding) the trigger
{
    my @got;
    react whenever Supply.from-list(1..7) {
        last if $_ > 4;
        @got.push($_);
    }
    is-deeply @got, [1, 2, 3, 4], 'last lets through values before the trigger';
}

# next inside a whenever over an on-demand supply
{
    my @got;
    react whenever supply { .emit for ^10 } {
        next if $_ > 4;
        @got.push($_);
    }
    is-deeply @got, [0, 1, 2, 3, 4], 'next works over an on-demand supply';
}

# last inside a whenever over an on-demand supply
{
    my @got;
    react whenever supply { .emit for ^10 } {
        last if $_ >= 3;
        @got.push($_);
    }
    is-deeply @got, [0, 1, 2], 'last works over an on-demand supply';
}

# A whenever that runs to completion still fires LAST (no topic required)
{
    my $ran = 0;
    react whenever Supply.from-list(1..3) {
        LAST { $ran++ }
    }
    is $ran, 1, 'LAST fires once on natural completion of the supply';
}
