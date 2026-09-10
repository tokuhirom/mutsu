use Test;

# A grammar's method resolution order threads through Grammar -> Match ->
# Capture -> Cool before Any/Mu (a Grammar is-a Match). mutsu detected
# grammar-ness by the presence of `token`/`rule` definitions, so an empty
# grammar, a token-less subclass, and the `Grammar` type object all dropped
# Match/Capture/Cool from `.^mro`.
# https://docs.raku.org/language/traps (`grammar G {}; G.^mro`).

plan 4;

sub mro-names($t) { $t.^mro.map(*.^name).join(' ') }

grammar Empty {}
is mro-names(Empty), 'Empty Grammar Match Capture Cool Any Mu',
    'an empty grammar threads Match/Capture/Cool';

grammar WithToken { token TOP { . } }
is mro-names(WithToken), 'WithToken Grammar Match Capture Cool Any Mu',
    'a grammar with a token keeps the full chain';

grammar Base {}
grammar Sub is Base {}
is mro-names(Sub), 'Sub Base Grammar Match Capture Cool Any Mu',
    'a token-less grammar subclass still threads Match/Capture/Cool';

is mro-names(Grammar), 'Grammar Match Capture Cool Any Mu',
    'the Grammar type object itself has the full chain';
