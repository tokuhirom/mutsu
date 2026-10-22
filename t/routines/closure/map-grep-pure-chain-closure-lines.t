use v6;
use Test;

plan 4;

# A closure body carries a line marker (#14164). The pure Int `.grep.map` chain
# batching (two bulk loops instead of per-element interleaving) must still
# recognise a one-statement body, and must not change the result.
my @data = 1..30;
is-deeply @data.grep({ $_ %% 3 }).map({ $_ * 2 }).list,
    (6, 12, 18, 24, 30, 36, 42, 48, 54, 60),
    'pure Int grep/map chain on one line';

is-deeply @data.grep({
    $_ %% 5
}).map({
    $_ + 1
}).list, (6, 11, 16, 21, 26, 31), 'pure Int grep/map chain across lines';

# An observable callback keeps its per-element interleaving.
my @log;
my @r = @data[^4].grep({ @log.push("g$_"); True }).map({ @log.push("m$_"); $_ }).list;
is-deeply @log, ['g1', 'm1', 'g2', 'm2', 'g3', 'm3', 'g4', 'm4'],
    'callbacks with side effects interleave per element';
is-deeply @r, [1, 2, 3, 4], 'and the values are unchanged';
