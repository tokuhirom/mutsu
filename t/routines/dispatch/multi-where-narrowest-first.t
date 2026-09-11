use Test;

# A `where` constraint is USER CODE, so when it runs is observable. Raku walks
# the candidates narrowest-first and stops at the first one that binds, which
# means a wider candidate's `where` never runs once a narrower one has matched.
# mutsu used to evaluate every candidate's constraint before ranking them,
# which both ran side effects raku never runs and made a `where`-heavy proto
# (Crane's 17-candidate `in`) cost a full constraint evaluation per candidate
# per call -- issue #7858.
#
# Every expectation below was verified against rakudo v2026.07.

plan 6;

# --- the narrower candidate binds: the wider one's `where` must not run ------

my @log;
multi sub f(Int:D $x where { @log.push('int'); True }) { 'int' }
multi sub f($x       where { @log.push('any'); True }) { 'any' }

is f(1), 'int', 'narrowest candidate wins';
is @log.grep('any').elems, 0,
   "a wider candidate's where does not run once a narrower one binds";

# --- the narrower candidate REJECTS: the wider one must still be reached -----

my @log2;
multi sub g(Int:D $x where { @log2.push('int'); False }) { 'int' }
multi sub g($x       where { @log2.push('any'); True })  { 'any' }

is g(1), 'any', 'a rejecting narrow candidate falls through to the wider one';
ok @log2.grep('any').elems > 0,
   "the wider candidate's where IS reached when the narrow one rejects";

# --- a `where` that dies is only reached if raku would have reached it -------

multi sub h(Int:D $x)                { 'int' }
multi sub h($x where { die 'boom' }) { 'any' }

is h(1), 'int',
   'a wider candidate whose where dies is never reached past a narrower match';

# The same constraint on the only applicable candidate still propagates.
multi sub i(Int:D $x where { die 'boom' }) { 'int' }
dies-ok { i(1) }, 'a where that dies on the reached candidate still throws';
