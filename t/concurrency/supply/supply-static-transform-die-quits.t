use Test;

# #12986: a `die` in a `.map`/`.grep` stage over a materialized supply
# (`Supply.from-list`) escaped the `react` instead of quitting the derived
# supply. The values before the failure still arrive, then the subscription
# quits: its QUIT phaser runs (LAST does not), or the react dies of it.

plan 7;

{
    my $caught;
    react {
        whenever Supply.from-list(1).map({ die 'boom' }) {
            QUIT { default { $caught = .message } }
        }
    }
    is $caught, 'boom', 'a die in .map reaches the whenever QUIT phaser';
}

{
    my ($caught, @seen);
    react {
        whenever Supply.from-list(1, 2, 3).grep({ $_ == 2 and die 'g'; True }) {
            @seen.push($_);
            QUIT { default { $caught = .message } }
            LAST { @seen.push('last') }
        }
    }
    is $caught, 'g', 'a die in .grep reaches the QUIT phaser';
    is-deeply @seen, [1], 'the values before the failure arrive, and LAST does not run';
}

{
    my @m;
    react {
        whenever Supply.from-list(1, 2, 3).map({ $_ == 3 ?? die('m') !! $_ * 10 }) {
            @m.push($_);
            QUIT { default { @m.push(.message) } }
        }
    }
    is-deeply @m, [10, 20, 'm'], 'mapped values, then the quit';
}

{
    my @got;
    Supply.from-list(1, 2).map({ $_ == 2 ?? die('t') !! $_ }).tap(
        { @got.push($_) }, quit => { @got.push('quit: ' ~ .message) });
    is-deeply @got, [1, 'quit: t'], 'a .tap quit handler sees it too';
}

{
    try { react { whenever Supply.from-list(1).map({ die 'unhandled' }) { } } }
    is $!.message, 'unhandled', 'without a QUIT phaser the react dies of it';
}

is-deeply Supply.from-list(1, 2, 3).map(* * 2).grep(* > 2).list, (4, 6),
    'stages that do not die are unchanged';
