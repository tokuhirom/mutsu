use Test;

# A qualified name on the LHS of `=>` (e.g. Bool::True, Order::Less) is a value,
# not an autoquoted string: `Bool::True => "a"` has the Bool VALUE as its key.
# And a Pair with a Bool-value key renders as `Bool::True => ...` (no parens),
# unlike a type object which renders as `(Int) => ...`.

plan 14;

# Qualified enum name key is the value, not a string
is (Bool::True => "a").key.WHAT.gist, "(Bool)", 'Bool::True key is a Bool';
is (Bool::False => "a").key.WHAT.gist, "(Bool)", 'Bool::False key is a Bool';
is (Order::Less => 1).key.WHAT.gist, "(Order)", 'Order::Less key is an Order';

# .raku rendering: Bool value key has no parens; type object key has parens
is (Bool::True => "a").raku, 'Bool::True => "a"', 'Bool::True Pair.raku';
is (Bool::False => "b").raku, 'Bool::False => "b"', 'Bool::False Pair.raku';
is (Int => 1).key.WHAT.gist, "(Str)", 'bare type name Int autoquotes to Str';
is Pair.new(Int, 1).raku, '(Int) => 1', 'type-object key keeps parens';
is Pair.new(Nil, 1).raku, '(Nil) => 1', 'Nil key keeps parens';

# Simple identifiers still autoquote (named-arg keys)
is (a => 1).key.WHAT.gist, "(Str)", 'simple name a autoquotes';
is (True => 1).raku, ':True(1)', 'simple True autoquotes to adverbial pair';

# A Bool value works as a hash key
my %h = Bool::True => "x";
is %h{True}, "x", 'Bool::True usable as a hash key';

# SetHash.antipairs (the roast case): the antipair key is Bool::True
my $sh = SetHash.new: <a b b c c c>;
is-deeply $sh.antipairs.sort(*.value),
    (Bool::True => "a", Bool::True => "b", Bool::True => "c"),
    'SetHash.antipairs keys are Bool::True';

# Named arguments unaffected
sub f(:$x) { $x }
is f(x => 5), 5, 'named arg x => 5 still binds';
sub g(:$verbose) { $verbose }
is g(:verbose), True, 'colonpair named arg';
