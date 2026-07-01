use Test;

plan 8;

sub l () { 1, 2 }

# `$(EXPR) = a, b` forces ITEM assignment: the `$(...)` contextualizer names the
# same container as EXPR but in item context, so the comma is NOT part of the RHS
# (`($(EXPR) = a), b`) -- unlike a bare `@a[0] = a, b`, which is list assignment.

# Expression context (parenthesized).
{
    my @a;
    my @z = ($(@a[0]) = l, l);
    is @a[0].elems, 2, '$(@a[0]) = l, l assigns just the first l() (item context)';
    is @z.elems,    2, 'the trailing comma extends the enclosing list';
    is @z[0].elems, 2, '@z[0] is the first l() = (1,2)';
    is @z[1].elems, 2, '@z[1] is the second l() = (1,2)';
}

# A parenthesized RHS is assigned whole.
{
    my @a;
    $(@a[0]) = (1, 2);
    is @a[0].elems, 2, '$(@a[0]) = (1, 2) assigns the whole parenthesized list';
}

# Statement context: item assignment, trailing comma is a sink sibling.
{
    my @a;
    $(@a[0]) = 1, 2;
    is @a[0], 1, '$(@a[0]) = 1, 2 (statement) assigns just 1';
}

# Bare `@a[0] = a, b` stays list assignment (comma slurped) -- unchanged.
{
    my @a;
    @a[0] = 1, 2;
    is @a[0].elems, 2, 'bare @a[0] = 1, 2 slurps the comma (list assignment)';
    my @z = (@a[0] = 3, 4);
    is @z.elems, 1, 'bare @a[0] = list, used as rvalue, is one itemized element';
}
