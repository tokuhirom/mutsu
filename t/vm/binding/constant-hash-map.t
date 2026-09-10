use v6;
use Test;

# A `%`-sigiled constant auto-coerces a non-Associative RHS (a List) to an
# immutable Map via `.Map` (terms.rakudoc), while an already-Associative RHS
# (a Hash literal or a Pair) is preserved as-is.

plan 11;

# List RHS -> Map
constant %foo = <foo bar>;
is %foo.WHAT.gist, '(Map)', 'constant % from a List is a Map';
is %foo.raku, 'Map.new((:foo("bar")))', 'Map .raku rendering';
is %foo<foo>, 'bar', 'Map value is readable';

# Odd/even list coercion
constant %pair = <a 1 b 2>;
is %pair.WHAT.gist, '(Map)', 'multi-element List constant is a Map';
is %pair<a>, '1', 'first key';
is %pair<b>, '2', 'second key';

# Hash literal RHS stays a mutable Hash
constant %bar = {:10foo, :72bar};
is %bar.WHAT.gist, '(Hash)', 'constant % from a Hash literal stays a Hash';
is %bar<foo>, 10, 'hash literal value';

# Pair RHS stays a Pair (already Associative)
constant %baz = :72baz;
is %baz.WHAT.gist, '(Pair)', 'constant % from a Pair stays a Pair';

# @-sigiled constant stays a List (unaffected by this fix)
constant @foo = 42;
is @foo.WHAT.gist, '(List)', 'constant @ is a List';
is @foo.raku, '(42,)', 'List constant .raku';
