use Test;

plan 41;

# #9162: `.first`, `.pick`/`.roll`, `.head`/`.tail`, `.skip`, `eqv`, `==`,
# `.gist`/`say`, `.List` and `.cache` used to copy the whole invocant to
# answer a question about one slot, the last k, a suffix or the length. They
# read the list in place now; these pin that the answers did not change.

# --- .first: a chunked scan must keep indices, :end and aliasing ---------
my @a = ^100;
is @a.first(* > 40), 41, '.first finds a hit past the first chunk';
is @a.first(* > 40, :k), 41, '.first(:k) reports the absolute index';
is @a.first(* %% 7, :end), 98, '.first(:end) scans from the end';
is @a.first(* < 3, :end, :k), 2, '.first(:end, :k) reports the absolute index';
is @a.first(* > 1000), Nil, '.first with no hit is Nil';
is (^100).List.first(* == 77, :p).gist, '77 => 77', '.first(:p) on a List';
is (^100).map(* * 2).first(* > 150), 152, '.first on a Seq';
my @w = 1, 2, 3;
@w.first({ $_ = 5 if $_ == 2; False });
is @w.gist, '[1 5 3]', '.first matcher topic still aliases the element';
my @shrink = ^50;
is @shrink.first({ @shrink.pop if $_ == 0; $_ == 48 }), 48,
    '.first survives a matcher that shrinks the array';

# --- .pick / .roll: one slot out of the borrowed list --------------------
my @p = <a b c>;
ok @p.pick (elem) @p, '.pick answers an element';
ok (@p.roll(5).all (elem) @p), '.roll(k) answers elements';
is @p.roll(5).elems, 5, '.roll(k) answers k elements';
is ().roll(3).elems, 0, '.roll(k) of an empty list is empty';
ok (<x y>.List.pick (elem) <x y>), '.pick on a List';

# --- .head / .tail ------------------------------------------------------
is (^10).List.tail(3).gist, '(7 8 9)', 'List.tail(k)';
is @a.tail, 99, 'Array.tail';
is @a.tail(2).gist, '(98 99)', 'Array.tail(k)';
is @a.tail(0).gist, '()', 'Array.tail(0)';
is @a.tail(1000).elems, 100, 'Array.tail(k) with k > elems';
is (1..5).map(* + 1).head(2).gist, '(2 3)', 'Seq.head(k)';

# --- .skip ---------------------------------------------------------------
is @a.skip(97).gist, '(97 98 99)', 'Array.skip(n)';
is @a.skip(*-2).gist, '(98 99)', 'Array.skip(callable)';
is (^10).List.skip.gist, '(1 2 3 4 5 6 7 8 9)', 'skip with no count drops one';
is (^10).List.skip(100).gist, '()', 'skip past the end is empty';

# --- eqv / == -----------------------------------------------------------
my @b = ^100;
my @c = ^101;
nok @a eqv @c, 'eqv: different lengths';
ok @a eqv @b, 'eqv: equal arrays';
nok [1, 2] eqv (1, 2), 'eqv: Array vs List';
@b[50] = 'x';
nok @a eqv @b, 'eqv: a difference in the middle';
my $fetched = 0;
my @proxied = 1, 2;
@proxied[1] := Proxy.new(FETCH => { $fetched++; 2 }, STORE => -> $, $ { });
ok @proxied eqv [1, 2], 'eqv FETCHes a Proxy element pair by pair';
ok @a == @b, '== compares lengths';
nok @a == @c, '== on different lengths';

# --- .gist / say: only the 100-element head matters ----------------------
my class Loud { method gist { 'LOUD' } }
my @g = ^300;
@g[250] = Loud.new;
is @g.gist.substr(*-19), '95 96 97 98 99 ...]', 'an instance past the head is not rendered';
@g[5] = Loud.new;
like @g.gist, /'3 4 LOUD 6'/, 'an instance inside the head still dispatches its gist';

# --- .List / .cache ------------------------------------------------------
my $l = (1, 2, 3);
ok $l.List eqv (1, 2, 3), 'List.List answers the List';
is $l.List.^name, 'List', 'List.List is a List';
is $(1, 2).List.raku, '(1, 2)', 'List.List drops the invocant itemization';
is (1, 2, 3).cache.^name, 'List', 'List.cache is a List';
my $s = (1..3).map(* * 2);
is $s.cache.gist, '(2 4 6)', 'Seq.cache answers its elements';

# --- .squish(:with): the eager pass is reverted, unrelated arrays untouched
my @big = ^10;
my @seen;
my $r = (1, 1, 2, 2, 3).squish(:with({ @seen.push($^a); $^a == $^b }));
is $r.List.gist, '(1 2 3)', 'squish(:with) result';
is @seen.gist, '[1 1 2 2]', 'squish(:with) callbacks ran once each';
is @big.elems, 10, 'an array the callback never names is left alone';
