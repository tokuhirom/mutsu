use Test;

# annotations' Keeper.hash builds `Map.new: ...` over Proxy-valued slots, and
# `%@keeper eqv Map.new(...)` was False: eqv resolved the nested Proxies by
# rebuilding the hash as a plain Hash, which dropped the Map's declared type.

plan 8;

sub fresh-proxy { Proxy.new(FETCH => -> $ { 1 }, STORE => -> $, $ {}) }

ok Map.new((a => fresh-proxy)) eqv Map.new((a => 1)), 'a Map with a Proxy value eqv a plain Map';
ok Map.new((a => 1)) eqv Map.new((a => fresh-proxy)), 'and the other way round';
nok Map.new((a => fresh-proxy)) eqv Map.new((a => 2)), 'a differing value is still not eqv';
ok (a => fresh-proxy).Map eqv (a => 1).Map, 'the .Map coercion';

my %h = a => fresh-proxy;
ok %h eqv {a => 1}, 'a Hash is unchanged';
nok Map.new((a => fresh-proxy)) eqv {a => 1}, 'a Map with a Proxy value is not a Hash';
ok Map.new((a => 1)).WHAT === Map, 'a plain Map is still a Map';
my $m = Map.new((a => fresh-proxy, b => 2));
ok $m eqv Map.new((a => 1, b => 2)), 'two keys';
