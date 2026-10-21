use Test;

# A `Proxy` argument (a `:=`-bound variable, a sigilless parameter) takes part
# in multi dispatch by what its FETCH answers: `multi f(%h)` beats `multi f(Mu)`
# for a Proxy over a Hash, and a `%`/`@` parameter binds the fetched container
# (#12907, PDF::Grammar::Test's `json-eqv`).
plan 8;

multi sub k(%a) { 'assoc' }
multi sub k(Mu $a) { 'mu' }
multi sub m(Hash $a) { 'hash' }
multi sub m(Mu $a) { 'mu' }
multi sub n(Str $a) { 'str' }
multi sub n(Mu $a) { 'mu' }

class T {
    has $!x = {a => 1};
    has $!s = 's';
    method x is rw { Proxy.new: FETCH => -> $ { $!x }, STORE => -> $, $v { $!x = $v } }
    method s is rw { Proxy.new: FETCH => -> $ { $!s }, STORE => -> $, $v { $!s = $v } }
}

my $t = T.new;
my $q := $t.x;
is k($q), 'assoc', 'a bound Proxy over a Hash binds %a';
is m($q), 'hash', 'and ranks Hash above Mu';
is m($t.x), 'hash', 'a direct accessor call too';
my $r := $t.s;
is n($r), 'str', 'a bound Proxy over a Str ranks Str above Mu';

sub w(\a) { k(a) }
is w($t.x), 'assoc', 'a sigilless parameter holding a Proxy';
sub w2(\a, |c) { m(a) }
is w2($t.x, 1), 'hash', 'also behind a capture parameter';

my $fetched = 0;
my $p := Proxy.new: FETCH => -> $ { $fetched++; {a => 1} }, STORE => -> $, $v { };
is k($p), 'assoc', 'a Proxy made in place';
ok $fetched > 0, 'its FETCH ran for the dispatch';
