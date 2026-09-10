use Test;

plan 6;

# Sigilless variable assignment in expression context (e.g., inside parentheses)
sub try-assign(\target, \value) {
    (try target = value)
}

# Immutable binding: assignment fails, try returns Nil
is try-assign(42, 999), Nil, 'try assign to immutable returns Nil';

# Mutable variable: assignment succeeds
my $x = 10;
is try-assign($x, 999), 999, 'try assign to mutable scalar returns assigned value';
is $x, 999, 'mutable scalar was updated';

# BareWord assignment in parens without try
sub assign-in-parens(\target) {
    my $result = (target = 100);
    $result
}
my $y = 0;
is assign-in-parens($y), 100, 'bareword assignment in parens returns value';
is $y, 100, 'variable was updated via bareword assignment in parens';

# Nested: (try target = value), value as argument list
sub multi-arg(\target, \value) {
    my @r = ((try target = value), value);
    @r
}
my $z = 0;
is-deeply multi-arg($z, 42), [42, 42],
    'bareword assignment in parens works as part of argument list';
