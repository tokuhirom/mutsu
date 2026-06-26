use Test;
plan 9;

# `$x ~~ (key => val)` runs `$x.key` and compares booleans: ?($x.key) === ?val
# (S03 Pair smartmatch). `key => val` literals build a ValuePair, which the
# smartmatch Pair handler must accept (it previously only matched Value::Pair).
{
    is ("abc" ~~ (chars => 3)), True,  'Str ~~ (chars => 3): ?3 === ?3';
    is ("abc" ~~ (chars => 0)), False, 'boolean semantics: ?3 === ?0 (False, 0 falsy)';
    is ("" ~~ (chars => 5)),    False, 'empty: ?0 === ?5 is False';
}

# User object: method dispatched, boolean-compared.
{
    class C { has $.n; method positive { $.n > 0 } }
    is (C.new(n=>5) ~~ (positive => True)),  True,  'obj ~~ (method => True) when method truthy';
    is (C.new(n=>-1) ~~ (positive => True)), False, 'obj ~~ (method => True) when method falsy';
    is (C.new(n=>-1) ~~ (positive => False)), True, 'obj ~~ (method => False) when method falsy';
}

# Numeric method returning a value, boolean-compared.
{
    class M { method size { 5 } }
    is (M.new ~~ (size => 1)), True,  '?(size=5) === ?1 is True';
    is (M.new ~~ (size => 0)), False, '?(size=5) === ?0 is False';
}

# A Hash LHS keeps its own Pair smartmatch (key+value), not method dispatch.
{
    my %h = a => 1, b => 2;
    is (%h ~~ (a => 1)), True, 'Hash ~~ Pair checks key+value, not method';
}
