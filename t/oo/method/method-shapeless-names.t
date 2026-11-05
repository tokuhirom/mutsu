use Test;

# ADR-15737: the class of a method name (takes an argument, Cool text method,
# Mu/Any-list/Iterable/container name, coercion) is memoized per symbol in
# `shapeless_row`. Behaviour must match the per-call classification it replaced,
# for names on each list, names on none, and a user method sharing a name.

plan 20;

my $x = 42;
is $x.WHAT.^name, 'Int', '.WHAT on an Int';
is $x.HOW.^name, 'Perl6::Metamodel::ClassHOW', '.HOW on an Int';
is $x.VAR.WHAT.^name, 'Scalar', '.VAR descriptor';
is Int.^name, 'Int', '.^name on a type object';

# names that take one argument vs. none
is (1, 2, 3).join('-'), '1-2-3', 'join with a separator';
is (1, 2, 3).head(2).join(','), '1,2', 'head with a count';
is (1, 2, 3).tail(2).join(','), '2,3', 'tail with a count';
is (1, 2, 3).head, 1, 'head without an argument';
is ((1, 2), 3).flat.elems, 3, 'flat';
is "a b c".words.elems, 3, 'words (Cool text method)';
is 12345.chars, 5, 'chars on an Int via Cool';
is "abc".comb.join(','), 'a,b,c', 'comb without an argument';
is "abcdef".comb(2).join(','), 'ab,cd,ef', 'comb with a count';

# Mu names
is 5.defined, True, 'defined';
is Int.defined, False, 'defined on a type object';
is (5.so, 0.not).join(','), 'True,True', 'so / not';

# Iterable / lazy / container names
is (1..5).lazy.is-lazy, True, '.lazy';
is [1, 2, 3].default.^name, 'Any', 'Array.default';

# a user method that shares an audited name wins on its own class
class Shadow {
    method join($sep) { "shadow-$sep" }
    method head { 'user-head' }
    method WHAT { 'user-what' }
}
is Shadow.new.join('x'), 'shadow-x', 'user join';
is Shadow.new.head, 'user-head', 'user head';
