use v6;
use Test;

# ValueMap (ecosystem): a user `is Map` class whose own STORE delegates to the
# native `Map.STORE` it fetched with `Map.^lookup('STORE')`.
plan 6;

my constant &base-new   = Map.^lookup('new');
my constant &base-store = Map.^lookup('STORE');

ok &base-store.defined, 'Map.^lookup(STORE) finds the native method';

my class MyMap is Map {
    method new(|c) { base-new(self, |c) }
    method STORE(+@values, :INITIALIZE($)!) {
        base-store self, @values, :INITIALIZE, :DECONT
    }
}

my %a is MyMap = a => 1, b => 2;
is %a.^name, 'MyMap', 'a variable tied with `is MyMap` is a MyMap';
is %a.elems, 2, 'the initializer reached the native STORE';
is-deeply %a<a b>, (1, 2), 'entries are readable';

my $direct = MyMap.new;
$direct.STORE((c => 3,), :INITIALIZE);
is $direct<c>, 3, 'calling STORE directly stores through the native method';

is-deeply %a.keys.sort.List, <a b>, 'keys';
