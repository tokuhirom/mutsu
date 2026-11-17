use v6;
use Test;

# The plain-method lane's early form (`try_plain_method_early_lane`) also
# serves a call with arguments: once one call has walked the whole dispatch
# chain, later calls with the same argument type keys skip the opcode-level
# probes. Every call below runs in a loop so the later iterations replay; each
# must answer what the full dispatch answers, including the call shapes the
# early lane refuses (a `|` spread, an `is rw` argument, an `AT-KEY` object
# key) and the argument effects it reproduces (a deferred `.map` reified).

plan 9;

class Acc {
    has @.seen;
    has %.h;
    method add($x) { @!seen.push($x); self }
    method two($a, $b) { "$a-$b" }
    method named(:$k = 'd') { $k }
    method bump($x is rw) { $x++ }
    method count(@xs) { @xs.elems }
    method AT-KEY($k) { %!h{$k} // "none:$k" }
}

my $o = Acc.new;
$o.add($_) for ^5;
is $o.seen.join(','), '0,1,2,3,4', 'a positional argument on every call';

is (^4).map({ $o.two($_, $_ * 2) }).join(' '), '0-0 1-2 2-4 3-6',
    'two positional arguments on every call';

is (^3).map({ $o.named(k => "v$_") }).join(','), 'v0,v1,v2', 'a named argument';
is (^3).map({ $o.named }).join(','), 'd,d,d', 'no argument next to the named calls';

my @pair = 'p', 'q';
is (^3).map({ $o.two(|@pair) }).join(','), 'p-q,p-q,p-q', 'a | spread at the call site';

my $n = 0;
$o.bump($n) for ^4;
is $n, 4, 'an is rw parameter writes back on every call';

is (^3).map({ $o.count((1..$_).map(* * 2)) }).join(','), '0,1,2',
    'a deferred .map argument is reified on every call';

class Key { method Str { 'k' } }
$o.h<k> = 'found';
is (^3).map({ $o.AT-KEY(Key.new) }).join(','), 'found,found,found',
    'an AT-KEY object key goes through its Str on every call';

my @types = (1, 'a', 2.5, 3, 'b');
is @types.map({ $o.two($_, $_.^name) }).join(' '), '1-Int a-Str 2.5-Rat 3-Int b-Str',
    'calls whose argument types alternate';
