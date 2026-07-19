use Test;

# The sequence operators (`...`/`…`/`...^`/`…^`) and list-infix meta-ops are
# LOOSER than comma, so on an assignment RHS the whole comma list is the
# sequence's seed and collapses into ONE sequence — the assignment-RHS twin of
# the argument-list precedence fix (#4755). This must hold for the Index-LHS
# (`@a[^5] = ...`) path, not only the declaration form (`my @a = ...`).

plan 10;

# Index-LHS slice assignment with a trailing sequence operator.
{
    my @a;
    @a[^5] = 1.5, 2.5 ... 5.5;
    is @a.raku, [1.5, 2.5, 3.5, 4.5, 5.5].raku, 'slice-assign: seed is the whole comma list';
}

# Declaration form still works (regression guard, routes through parse_comma_or_expr).
{
    my @a = 1.5, 2.5 ... 5.5;
    is @a.raku, [1.5, 2.5, 3.5, 4.5, 5.5].raku, 'my @a = : declaration form unchanged';
}

# Plain `@a = ...` (no index) unchanged.
{
    my @a;
    @a = 1.5, 2.5 ... 5.5;
    is @a.raku, [1.5, 2.5, 3.5, 4.5, 5.5].raku, '@a = : plain assignment unchanged';
}

# Integer sequence via index-LHS assignment.
{
    my @a;
    @a[^6] = 1, 3 ... 11;
    is @a.raku, [1, 3, 5, 7, 9, 11].raku, 'integer seed list 1, 3 ... 11';
}

# Exclusive endpoint on the RHS.
{
    my @a;
    @a[^5] = 1, 2 ...^ 6;
    is @a.raku, [1, 2, 3, 4, 5].raku, 'exclusive endpoint ...^';
}

# A list-infix meta-op is also looser than comma on the RHS.
{
    my @a;
    @a[^2] = 1, 2 Z+ 10, 20;
    is @a.raku, [11, 22].raku, 'Z+ meta-op absorbs the whole comma level';
}

# A plain comma list (no top-level sequence op) is untouched.
{
    my @a;
    @a[^3] = 1, 2, 3;
    is @a.raku, [1, 2, 3].raku, 'plain comma list unaffected';
}

# A single element with a trailing comma stays a 1-element list (Range kept).
{
    my @a;
    @a[0] = 1..5,;
    is @a.raku, '[(1..5,),]', 'trailing comma keeps a 1-element list';
}

# Multiple whole-array assignments in a for-loop body (index reuse).
{
    my @out;
    for 0..0 {
        @out[^3] = 10, 20 ... 30;
    }
    is @out.raku, [10, 20, 30].raku, 'sequence in loop-body index assignment';
}

# Hash-index LHS also collapses the seed list.
{
    my %h;
    %h<x> = 1, 3 ... 7;
    is %h<x>.join(','), '1,3,5,7', 'hash-index assignment collapses the seed list';
}
