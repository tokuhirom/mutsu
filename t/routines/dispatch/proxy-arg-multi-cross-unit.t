use lib 't/lib';
use Test;

# A multi dispatcher value called from another compunit with a Proxy argument
# ranks AND matches its candidates on what the Proxy's FETCH answers, so a
# `%a` candidate takes a Proxy over a Hash. Matching used to see the Proxy
# itself and fell through to the `Mu` candidate. Found by PDF 0.6.17's
# `t/indobj-array.t`. Expected values are the `raku` (v2026.09) output.

use ProxyArgMultiFamily;

plan 4;

my $hash = { x => 1 };
my $list = [1, 2];
my $hp := Proxy.new(FETCH => -> $ { $hash }, STORE => -> $, \n { $hash = n });
my $lp := Proxy.new(FETCH => -> $ { $list }, STORE => -> $, \n { $list = n });

is run($hp, { a => 1 }), 'hash', 'a Proxy over a Hash matches the %-sigil candidate';
is run($lp, [1]), 'list', 'a Proxy over a List matches the List:D candidate';
is run($hash, { a => 1 }), 'hash', 'a plain Hash matches it too';
is run(5, 6), 'mu', 'anything else takes the Mu candidate';
