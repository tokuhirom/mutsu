use v6;
use Test;

# From Hash::LRU (`my $cache = my % is LRU(elements => 3)`): a Hash with a role
# mixed in that overrides AT-KEY / ASSIGN-KEY. A key-list slice store is one
# ASSIGN-KEY per key, and `++%h<k>` / `%h<k>--` call AT-KEY once and bump the
# container it returns (no ASSIGN-KEY), rather than the backing storage.
plan 12;

my @log;
my role Logged {
    method AT-KEY(::?CLASS:D: \key) is raw { @log.push: "at {key}"; callsame }
    method ASSIGN-KEY(::?CLASS:D: \key, \value) { @log.push: "set {key}"; callwith(key, value) }
}
my $h = my % does Logged;

is ($h<a b c> = 1, 2, 3), (1, 2, 3), 'a key-list slice store answers the values';
is-deeply @log, ['set a', 'set b', 'set c'], 'one ASSIGN-KEY per key';
is-deeply $h.keys.sort.List, <a b c>, 'every key was stored';
is $h<b>, 2, 'and each took its own value';

@log = ();
is ++$h<b>, 3, 'a prefix increment answers the new value';
is-deeply @log, ['at b'], 'it called AT-KEY once and never ASSIGN-KEY';
is $h<b>, 3, 'and the element holds it';

is $h<c>--, 3, 'a postfix decrement answers the old value';
is $h<c>, 2, 'and the element holds the new one';

$h<x> = 10;
is ++$h<x>, 11, 'a counter set through ASSIGN-KEY increments';

$h<n>++;
is $h<n>, 1, 'an absent element increments from zero';

my $short = my % does Logged;
$short<p q r> = 1, 2;
is-deeply $short.keys.sort.List, <p q r>, 'keys past the values are still stored';
