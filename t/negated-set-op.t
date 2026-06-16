use Test;

plan 26;

my $a = (1, 2);
my $b = (1, 2, 3);

# Negated set membership: !(elem) / ∉
ok !(9 (elem) $b),       '9 not (elem) b is true via inner';
ok 9 !(elem) $b,         '9 !(elem) b';
nok 2 !(elem) $b,        '2 !(elem) b is false';
ok 9 ∉ $b,               '9 ∉ b';
nok 2 ∉ $b,              '2 ∉ b is false';

# Negated containment: !(cont) / ∌
ok $b !(cont) 9,         'b !(cont) 9';
nok $b !(cont) 2,        'b !(cont) 2 is false';
ok $b ∌ 9,               'b ∌ 9';
nok $b ∌ 2,              'b ∌ 2 is false';

# Negated subset: !(<=) / ⊈
nok $a !(<=) $b,         'a !(<=) b is false (a is subset)';
ok $b !(<=) $a,          'b !(<=) a is true';
ok $b ⊈ $a,              'b ⊈ a';
nok $a ⊈ $b,             'a ⊈ b is false';

# Negated superset: !(>=) / ⊉
ok $a !(>=) $b,          'a !(>=) b is true';
nok $b !(>=) $a,         'b !(>=) a is false (b is superset)';
ok $a ⊉ $b,              'a ⊉ b';
nok $b ⊉ $a,             'b ⊉ a is false';

# Negated strict subset: !(<) / ⊄
nok $a !(<) $b,          'a !(<) b is false';
ok $a !(>) $b,           'a !(>) b is true';

# Negated set equality: !(==) / ≢
ok $a !(==) $b,          'a !(==) b is true';
nok $a !(==) $a,         'a !(==) a is false';
ok $a ≢ $b,              'a ≢ b';
nok $a ≢ $a,             'a ≢ a is false';

# Works with Set objects too
my $s = set(1, 2, 3);
ok 9 !(elem) $s,         '9 !(elem) set';
nok 1 !(elem) $s,        '1 !(elem) set is false';

# Chained / combined with boolean
ok (9 !(elem) $b) && (8 !(elem) $b), 'two negated memberships combined';
