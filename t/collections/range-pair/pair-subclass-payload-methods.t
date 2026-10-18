use Test;

# A subclass of `Pair` inherits every method of its base: a method the class
# does not declare runs on the `Pair` it carries (ADR-11276 §9.180), its
# string coercions included, while its own methods and accessors win.

plan 14;

class P is Pair { method extra { 'x' } }
my $p = P.new(:key<a>, :value(3));

is $p.key, 'a', 'key';
is $p.value, 3, 'value';
is-deeply $p.kv, ('a', 3), 'kv';
is $p.antipair.key, 3, 'antipair';
is $p.Str, "a\t3", 'Str is the Pair\'s';
is ~$p, "a\t3", 'prefix ~ too';
is-deeply $p.Hash, {a => 3}, 'Hash (Any.Hash on the Pair)';
is $p.Map.WHAT, Map, 'Map';
is $p.elems, 1, 'elems';
is $p.fmt('%s:%s'), 'a:3', 'fmt';
is $p.extra, 'x', "the class's own method";
isa-ok $p, P, 'the instance keeps its class';

class Q is Pair { method value { 42 } }
my $q = Q.new(:key<k>, :value(1));
is $q.value, 42, 'an overridden accessor wins';
is $q.key, 'k', 'the others still answer on the Pair';
