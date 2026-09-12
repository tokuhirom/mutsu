# A role mixin WRAPS a value without replacing it: rakudo's `%h does R` is a
# `Hash+{R}`, still a Hash, and `@a but R` is still an Array. So `.Set`/`.Bag`/
# `.Mix` (and their mutable variants) and every set operator must fold the inner
# value's ELEMENTS. mutsu used to fall through to the "unknown scalar" arm for a
# mixin and contribute the whole hash as ONE element, so `%h (-) %allowed` never
# cancelled anything and `%h.Set` answered `Set.new({:a(42), :b(0)})`.
#
# From Hash::Restricted 0.0.9, whose restricting roles do `self (-) %allowed`
# inside their own `STORE` with `self` a role-mixed hash.

use Test;

plan 23;

role Tagged { method tagged() { True } }

my %m does Tagged = a => 42, b => 0;
my @l does Tagged = 1, 2, 2;

# --- coercion methods on a mixin --------------------------------------------
# `b => 0` is a falsy value, so a Setty coercion drops it; a Baggy one keeps
# the value as the weight.
is-deeply %m.Set, set('a'), 'Hash+{R}.Set folds the hash keys (falsy value dropped)';
is-deeply %m.Bag, ('a' => 42).Bag, 'Hash+{R}.Bag uses the hash values as weights';
is-deeply %m.Mix, ('a' => 42).Mix, 'Hash+{R}.Mix uses the hash values as weights';
is %m.SetHash.elems, 1, 'Hash+{R}.SetHash folds the hash keys';
is %m.BagHash<a>, 42, 'Hash+{R}.BagHash keeps the weight';
is %m.MixHash<a>, 42, 'Hash+{R}.MixHash keeps the weight';

is-deeply @l.Set, set(1, 2), 'Array+{R}.Set folds the elements';
is-deeply @l.Bag, bag(1, 2, 2), 'Array+{R}.Bag counts the elements';

# --- set operators over a mixin ---------------------------------------------
is-deeply %m (-) set('a'), set(), 'Hash+{R} (-) $set cancels the shared key';
is-deeply %m (-) set('z'), set('a'), 'Hash+{R} (-) $set keeps an unmatched key';
is-deeply %m (|) set('z'), set('a', 'z'), 'Hash+{R} (|) $set unions the keys';
is-deeply %m (&) set('a'), set('a'), 'Hash+{R} (&) $set intersects the keys';
is-deeply %m (^) set('z'), set('a', 'z'), 'Hash+{R} (^) $set is the symmetric difference';
is-deeply @l (|) set('z'), set(1, 2, 'z'), 'Array+{R} (|) $set unions the elements';
ok 'a' (elem) %m, 'Hash+{R} is a membership container over its keys';

# Baggy arithmetic reads the hash values as weights, so `(+)` adds them and
# `(.)` multiplies -- both are 1-weighted against a plain `bag('a')`.
is (%m (+) bag('a'))<a>, 43, 'Hash+{R} (+) $bag adds the weights';
is (%m (.) bag('a'))<a>, 42, 'Hash+{R} (.) $bag multiplies the weights';
is (%m (&) set('a')).elems, 1, 'Hash+{R} (&) $set keeps exactly the shared key';

# --- what must NOT be unwrapped --------------------------------------------
# An allomorph is a mixin too (`<1>` wraps `Int(1)` with a `Str` override), and
# its whole point is to be a DISTINCT element from the value it wraps. Only a
# role mixin may be folded through.
is (1, "1", 1.0, <1>).Set.elems, 4,
  'an allomorph stays a distinct Set element from the value it wraps';
is (<1>,).Set.keys[0].Str, '1', 'the allomorph element survives the coercion';
is (<1> (|) set('z')).elems, 2, 'an allomorph operand is one element, not its inner value';

# A role-mixed SCALAR element keeps its own identity, but a role-mixed
# AGGREGATE element flattens in list context exactly as the bare one would.
my $scalar-mixin = 5 but Tagged;
is (5, $scalar-mixin).Set.elems, 2,
  'a role-mixed scalar element is distinct from the value it wraps';
is-deeply (1, %m).Set, set(1, 'a'),
  'a role-mixed hash element flattens its keys in list context';
