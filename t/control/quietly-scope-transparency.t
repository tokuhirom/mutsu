use Test;

plan 9;

# Statement-prefix `quietly` is scope-transparent: a `my` declaration inside it
# leaks to the enclosing scope (Raku semantics), unlike a block/sub.
quietly my $a = 5;
is $a, 5, 'quietly my $a = 5 leaks $a to the enclosing scope';

quietly my @b = 1, 2, 3;
is @b.elems, 3, 'quietly my @b = ... leaks @b';

quietly my %h = :x(1), :y(2);
is %h<x>, 1, 'quietly my %h = ... leaks %h';

# A warning raised while evaluating the guarded expression is suppressed, and
# the warn resumes in place so the assignment still completes with its value.
quietly my $n = Mu.Numeric;
is $n, 0, 'quietly suppresses the warn AND the assignment completes (Mu.Numeric)';

quietly my \z .= Numeric;
is z, 0, 'quietly my \z .= Numeric leaks z and resumes to 0';

# The block form still works and suppresses warnings.
my $ran = 0;
quietly { warn "boom"; $ran = 1 };
is $ran, 1, 'quietly { ... } runs the block with warnings suppressed';

# A plain expression under quietly returns its value.
is (quietly 41 + 1), 42, 'quietly EXPR returns the expression value';

# quietly does not suppress non-warning output.
{
    my $x;
    quietly $x = 7;
    is $x, 7, 'quietly assignment to an outer var works';
}

# Nested quietly is fine.
quietly { quietly my $deep = 99; is $deep, 99, 'nested quietly leaks inner decl' };
