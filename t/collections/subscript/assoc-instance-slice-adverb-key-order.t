use Test;

# A whatever slice (`%h{*}`) and a zen slice (`%h{}`) name every element of an
# Associative, and the order is the one the object's own `keys` method returns.
#
# For a user Associative mutsu snapshots `keys` + `AT-KEY` into a plain Hash and
# runs the ordinary Hash slice path on it. The `*`/zen expansion then read the
# SNAPSHOT's key order, which has nothing to do with `keys`, so an ordered
# Associative got its values shuffled -- `%h{*}:v` disagreed with both
# `%h.values` and the equivalent explicit key slice. Found via the
# Hash::Ordered zef distribution ("does a value zen-slice work").

plan 8;

class Ordered does Associative {
    has @.ks;
    method keys()         { @!ks }
    method AT-KEY($k)     { $k.uc }
    method EXISTS-KEY($k) { so @!ks.first(* eq $k) }
    method values()       { @!ks.map({ self.AT-KEY($_) }) }
}

# Deliberately not sorted, and not insertion-order-of-a-plain-hash either.
my $o = Ordered.new(ks => <d b a c>);

is-deeply $o.keys.List, <d b a c>, 'the object reports its own key order';
is-deeply ($o{<d b a c>}:v).List, ('D', 'B', 'A', 'C'),
  'an explicit key slice keeps the order it was given';
is-deeply ($o{*}:v).List, ('D', 'B', 'A', 'C'),
  'a whatever value slice follows the object keys order';
is-deeply ($o{}:v).List, ('D', 'B', 'A', 'C'),
  'a zen value slice follows the object keys order';
is-deeply ($o{*}:k).List, ('d', 'b', 'a', 'c'),
  'a whatever key slice follows the object keys order';
is-deeply ($o{*}:kv).List, ('d', 'D', 'b', 'B', 'a', 'A', 'c', 'C'),
  'a whatever :kv slice pairs keys and values in the object order';
is-deeply ($o{*}:p).List, (d => 'D', b => 'B', a => 'A', c => 'C'),
  'a whatever :p slice pairs keys and values in the object order';

# The plain (adverbless) whatever slice already followed `keys`; keep the two
# routes pinned together.
is-deeply ($o{*}).List, ('D', 'B', 'A', 'C'),
  'the adverbless whatever slice agrees with the :v one';
