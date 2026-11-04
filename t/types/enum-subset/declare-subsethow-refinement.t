use Test;
use lib 't/lib-declare-subsethow';
use DeclareProto;

# From the Protocol distribution: a DECLARE'd declarator whose HOW is a
# Metamodel::SubsetHOW subclass makes the declared name a subset, refined by
# the predicate its `new_type` override hands to `callwith`.
plan 7;

protocol Writer { method write { ... } }

class W { method write {} }
class Other { method read {} }

ok W ~~ Writer, 'a type object with the methods matches the protocol';
ok W.new ~~ Writer, 'an instance with the methods matches';
nok Other ~~ Writer, 'a class without them does not';
nok 42 ~~ Writer, 'neither does an Int';

my Writer $w;
lives-ok { $w = W.new }, 'a matching object can be assigned to a Writer variable';
dies-ok { $w = 42 }, 'a non-matching one cannot';

multi pick(Writer $x) { 'writer' }
multi pick($x) { 'other' }
is-deeply (pick(W.new), pick(7)), ('writer', 'other'), 'multi dispatch sees the subset';
