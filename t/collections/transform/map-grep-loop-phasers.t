use Test;

# A `.map` / `.grep` block iterates like a loop body: its `FIRST` phaser runs
# once, before the first call, and a map block's `LAST` runs once after the
# last call. Rakudo runs no `LAST` for a grep block. mutsu never ran `FIRST`,
# and ran no phaser at all for a map over an array (#14299). Expected values
# are the `raku` (v2026.09) output.

plan 11;

sub trace(&code) { my $*T = ''; code(); $*T }

is trace({ (1..3).map({ FIRST { $*T ~= 'F' }; LAST { $*T ~= 'L' }; $_ }).eager }),
    'FL', 'a map block runs FIRST and LAST once';
is trace({ (1..3).grep({ FIRST { $*T ~= 'F' }; LAST { $*T ~= 'L' }; True }).eager }),
    'F', 'a grep block runs FIRST only';
my @a = 1..3;
is trace({ @a.map({ FIRST { $*T ~= 'F' }; LAST { $*T ~= 'L' }; $_ }).eager }),
    'FL', 'a map over an array runs both';
is trace({ @a.grep({ FIRST { $*T ~= 'F' }; LAST { $*T ~= 'L' }; True }).eager }),
    'F', 'a grep over an array runs FIRST only';
is trace({ (1..4).map(-> $a, $b { FIRST { $*T ~= 'F' }; LAST { $*T ~= 'L' }; $a }).eager }),
    'FL', 'a multi-parameter map block runs both';
is trace({ (1..3).map(-> Int $x { FIRST { $*T ~= 'F' }; LAST { $*T ~= 'L' }; $x }).eager }),
    'FL', 'a typed-parameter map block runs both';
is trace({ (1..3).map({ FIRST { $*T ~= 'F' }; LAST { $*T ~= 'L' }; last if $_ == 2; $_ }).eager }),
    'FL', 'LAST runs after a `last`';
is trace({ (1..*).map({ FIRST { $*T ~= 'F' }; $_ }).head(2).eager }),
    'F', 'a lazy map over an infinite source runs FIRST once';
is trace({ my $s = (1..5).map({ FIRST { $*T ~= 'F' }; $_ }); $s[0]; $s[3] }),
    'F', 'FIRST runs once across reads of a deferred map';
is trace({ (^0).map({ FIRST { $*T ~= 'F' }; $_ }).eager }),
    '', 'a map over nothing runs no FIRST';

my $n = 0;
(1..3).map({ FIRST { $n += 10 }; $_ }).eager;
is $n, 10, 'a FIRST phaser writes a captured lexical';
