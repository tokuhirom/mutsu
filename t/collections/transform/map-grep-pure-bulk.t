use Test;

plan 5;

my @data = 1..30;
is-deeply @data.grep({ $_ %% 3 }).map({ $_ * 2 }).List,
    (6, 12, 18, 24, 30, 36, 42, 48, 54, 60),
    'a full pull of integer grep and map preserves every result';
is-deeply @data.grep({ $_ + 1 }).map({ $_ - 1 }).List,
    (0..29).List, 'other integer arithmetic expressions retain their values';
is @data[2], 3, 'consuming the chain leaves the source value intact';

my @events;
is-deeply @data.grep({ @events.push("g$_"); $_ %% 10 })
    .map({ @events.push("m$_"); $_ * 2 }).List,
    (20, 40, 60), 'effectful callbacks still produce the expected result';
is-deeply @events[0..11],
    <g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 m10 g11>,
    'effectful callbacks still interleave';
