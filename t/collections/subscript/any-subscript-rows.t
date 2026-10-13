use Test;

# ADR-11276 §9.106: `Any`'s subscript protocol is a row. A value that is not
# `Positional` is the one-element list holding itself; it has no keys.

plan 29;

class C { }
class N { method Int { 0 } }

# AT-POS / EXISTS-POS of a defined value.
is 5.AT-POS(0), 5, 'Int.AT-POS(0) is the value';
isa-ok 5.AT-POS(1), Failure, 'Int.AT-POS(1) is out of range';
is 5.AT-POS(N.new), 5, 'an index is read as its .Int';
ok 5.EXISTS-POS(0), 'Int.EXISTS-POS(0)';
nok 5.EXISTS-POS(1), 'Int.EXISTS-POS(1)';
ok 5.EXISTS-POS("0"), 'a Str index is `pos.Int == 0`';
is "abc".AT-POS(0), "abc", 'a Str is not Positional';
nok "abc".EXISTS-POS(1), 'Str.EXISTS-POS(1)';
is C.new.AT-POS(0).^name, 'C', 'an instance is its own element 0';
is C.new.AT-POS(N.new).^name, 'C', '... with an index read as its .Int';
ok C.new.EXISTS-POS(0), 'an instance has element 0';
is-deeply {a => 1}.AT-POS(0), {a => 1}, 'a Hash is not Positional';
is set(1).AT-POS(0), set(1), 'a Set is not Positional';
ok (a => 1).EXISTS-POS(0), 'a Pair is not Positional';

# A type object answers itself at 0 but holds no element.
is Int.AT-POS(0), Int, 'Int.AT-POS(0) is the type object';
is C.AT-POS(0), C, 'a user type object too';
nok Int.EXISTS-POS(0), 'a type object holds no element';
nok Int.EXISTS-POS("0"), '... whatever the index';
throws-like { Int.EXISTS-POS(Int) }, Exception, message => "Cannot use 'Int' as an index",
    'a type object is no index';

# AT-KEY / EXISTS-KEY.
is 1.5.AT-KEY("x").exception.message, 'Type Rat does not support associative indexing.',
    'a defined value fails the associative subscript';
is C.new.AT-KEY("x").exception.message, 'Type C does not support associative indexing.',
    '... an instance names its class';
is Int.AT-KEY("x"), Any, 'a type object autovivifies to Any';
is C.AT-KEY("x"), Any, '... a user type object too';
nok 5.EXISTS-KEY("x"), 'a value has no keys';
nok C.new.EXISTS-KEY("x"), 'an instance has no keys';
nok Str.EXISTS-KEY("x"), 'a type object has no keys';

# A list with no settled shape still indexes its elements.
is (1..3).lazy.AT-POS(1), 2, 'a lazy Seq indexes its elements';

# A `Match` is a `Capture`: its positional captures.
my $m = "x" ~~ /(.)(.)?/;
nok $m.EXISTS-POS(1), 'Match.EXISTS-POS: an optional group that did not match';
is ~$m.AT-POS("0"), 'x', 'Match.AT-POS coerces its index';
