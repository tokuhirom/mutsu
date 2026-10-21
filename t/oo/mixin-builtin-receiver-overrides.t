use Test;

plan 16;

is (5 but False).Bool, False, 'a Bool mixin overrides .Bool';
is ("abc" but "xyz").Str, 'xyz', 'a Str mixin overrides .Str';
is (5 but "x").gist, 'x', 'a Str mixin wins for .gist';
is (True but False).Str, 'False', 'True but False is False for .Str';
is (True but False).Int, 0, 'True but False is False for .Int';
ok (True but False).not, 'True but False .not';
is (True but [1, 2]).Array.elems, 2, 'a mixin key named after the method answers';
is (5 but False).clone.Bool, False, '.clone keeps the mixin';

role R { method r { 'r' } }
is (5 but R).list[0].r, 'r', 'a role mixin over a scalar is its own .list element';
is (5 but R).List.elems, 1, '.List of a scalar role mixin';
is (Set.new("a") but R).raku, 'Set+{R}.new("a")', 'a Set role mixin names the role in .raku';
is (Set.new("a") but R).gist, 'Set+{R}(a)', 'and in .gist';
is (Any but role Meows { }).gist, '(Any+{Meows})', 'a type object names its composed type';
is (<a b> but R).flat.elems, 2, '.flat runs over the whole mixin';
is (5 but R).abs, 5, 'any other method is the wrapped value\'s';
is (<1e3>).gist, '1e3', 'an allomorph gists as its source string';
