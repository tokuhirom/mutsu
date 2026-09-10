use Test;

# Calling `.new(...)` on a *concrete instance* of a builtin type delegates to
# that type's constructor, exactly as raku does (e.g. inside the `.=` metaop
# `($p = ...).=new :key<k> :value<v>` — roast S03-operators/inplace.t).

plan 9;

# Pair instance .new builds a fresh Pair from key/value named args.
my $p = Pair.new(:key<a>, :value<b>);
is-deeply $p.new(:key<foo>, :value<bar>), (:foo<bar>).Pair,
    'Pair instance .new(:key, :value) builds a new Pair';

# Numeric / Str instances delegate to their type's default constructor.
is-deeply 42.new,    0,   'Int instance .new => 0';
is-deeply (4.2).new, 0.0, 'Rat instance .new => 0.0';
is-deeply (4e0).new, 0e0, 'Num instance .new => 0e0';
is-deeply "s".new,   "",  'Str instance .new => ""';

# The `.=` metaop on a parenthesised assignment target works end-to-end.
my $q;
($q = $p.antipair.antipair).=new :key<foo> :value<bar>;
is-deeply $q, (:foo<bar>).Pair, '(.= new) with fake-infix adverbs on paren-assign target';

# A bigint instance still delegates to Int.
is-deeply (10 ** 30).new, 0, 'BigInt instance .new => 0';

# Delegation does not disturb type-object construction.
is-deeply Pair.new(:key<x>, :value<y>), (:x<y>).Pair, 'Pair type .new still works';
is-deeply Int.new, 0, 'Int type .new still works';
