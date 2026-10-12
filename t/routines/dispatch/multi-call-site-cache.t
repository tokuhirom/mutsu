use Test;

# A bare multi call whose winner is a function of its argument types goes
# straight to the winner once the types were seen (#10109). Every call below
# repeats, so the second and later ones take that cached path; each pins
# something the cache must not change.

plan 8;

multi kind(Int:D $x) { 'Int' }
multi kind(Str $x)   { 'Str' }
multi kind(Int:U $x) { 'Int:U' }
is (5, 'a', 6, Int, 'b', 7, Int).map({ kind($_) }).join(' '),
    'Int Str Int Int:U Str Int Int:U',
    'alternating argument types each reach their own candidate';

multi nx(Int $x) { 'int+' ~ callsame() }
multi nx(Any $x) { 'any' }
is (^3).map({ nx($_) }).join(' '), 'int+any int+any int+any',
    'callsame from a cached winner reaches the next candidate';

multi sw(Int $x) { $x > 0 ?? samewith($x - 1) !! 'done' }
multi sw(Str $x) { 'str' }
is (^3).map({ sw(2) }).join(' '), 'done done done', 'samewith from a cached winner';

multi grow(Int $x) { 'Int' }
multi grow(Any $x) { 'Any' }
my @grown = (^2).map({ grow(1) });
{
    multi grow(Int $x where * > 0) { 'positive' }
    @grown.append: (^2).map({ grow(1) });
}
@grown.append: (^2).map({ grow(1) });
is @grown.join(' '), 'Int Int positive positive Int Int',
    'a candidate an inner scope adds takes over the cached winner there only';

multi wr(Int $x) { "w$x" }
multi wr(Str $x) { "s$x" }
my @plain = (^2).map({ wr(1) });
&wr.wrap(-> |c { 'wrapped:' ~ callsame() });
is (|@plain, |(^2).map({ wr(1) })).join(' '), 'w1 w1 wrapped:w1 wrapped:w1',
    'a wrapper installed after the winner was cached runs';

multi rw(Int $x is rw) { $x++ }
multi rw(Str $x) { 'str' }
my $n = 1;
rw($n) for ^3;
is $n, 4, 'an is-rw candidate writes the caller variable on every call';

multi thrower(Int $x) { die "boom $x" }
multi thrower(Str $x) { 'ok' }
is (^3).map({ (try thrower(1)) // $!.message }).join(' '), 'boom 1 boom 1 boom 1',
    'an exception from a cached winner propagates';

my $acc = 0;
multi bump(Int $x) { $acc += $x }
multi bump(Str $x) { 0 }
bump(2) for ^3;
is $acc, 6, 'a cached winner writes an outer lexical';
