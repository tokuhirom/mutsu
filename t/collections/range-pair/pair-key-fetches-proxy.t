use Test;

plan 6;

# The key of `=>` is a decontainerized `Mu $key`: a Proxy key is FETCHed once,
# whether the pair is built by `=>` or by the `Z=>` zip.
my $k := Proxy.new(FETCH => method { "a" }, STORE => method (Mu $) { });

is ($k => 1).key, 'a', '=> fetches a Proxy key';
is-deeply Map.new(($k => 1)), Map.new(a => 1), 'the Map is keyed by the fetched value';

my @l = ($k,);
is-deeply (@l Z=> (1,)).List, (a => 1,), 'Z=> fetches a Proxy key';
is-deeply Map.new(reverse @l Z=> (1,)), Map.new(a => 1), 'Map.new over Z=> is keyed by it';

my @bound;
@bound.BIND-POS(0, $k);
is-deeply Map.new(@bound.head(1) Z=> (1,)), Map.new(a => 1), 'a BIND-POS-bound slot is fetched too';

my %h = ($k => 2);
is-deeply %h, {a => 2}, 'a hash built from the pair is keyed by the fetched value';
