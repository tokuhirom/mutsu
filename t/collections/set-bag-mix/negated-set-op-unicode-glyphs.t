use Test;

# The `!` meta-prefix negates any Bool-returning infix, so it composes with the
# Unicode spellings of the set relations, not only their ASCII `(elem)` forms.
# Math::Symbolic writes its variable filter with the glyph rather than
# `!(elem)`, and the whole module used to fail to parse on it. The glyphs below
# are the operators under test, so they are the one thing here that cannot be
# spelled in ASCII.

plan 14;

ok  1 !(elem) (2, 3), 'ASCII !(elem) still works';
ok  1 !∈ (2, 3), 'negated element-of with the glyph';
nok 1 !∈ (1, 3), 'and it is false when the element is there';

ok  (1, 2) !∋ 3, 'negated contains with the glyph';
nok (1, 2) !∋ 1, 'and false when it does contain';

ok  (1, 2) !⊆ (1,), 'negated subset with the glyph';
nok (1,) !⊆ (1, 2), 'and false for a real subset';

ok  (1,) !⊇ (1, 2), 'negated superset with the glyph';
nok (1, 2) !⊇ (1,), 'and false for a real superset';

ok  (1, 2) !⊂ (1, 2), 'negated strict subset with the glyph';
ok  (1, 2) !⊃ (1, 2), 'negated strict superset with the glyph';

# The precomposed negated glyph keeps working, and agrees with the `!` form.
ok  1 ∉ (2, 3), 'the precomposed negated glyph still works';

# Inside a whatever-curry, which is how Math::Symbolic reaches it.
my @vars = 1, 2, 3;
my @keep = @vars.grep: * !∈ (2,);
is-deeply @keep, [1, 3], 'a negated glyph operator curries with *';

# The non-Bool set operators are not negatable, so `!` before one is not an
# operator at all.
nok (try EVAL 'say (1,) !∪ (2,)').defined,
    'a non-Bool set operator is still not negatable';
