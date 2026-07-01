use Test;

plan 10;

# A chained list assignment where the *inner* target is an `@`/`%` container
# absorbs the comma list on its right, so `@a = %h = 1, 2` parses as
# `@a = (%h = (1, 2))`, not `(@a = (%h = 1)), 2`.

{
    my (@a, @b, %h);
    @a = %h = 1, 2;
    @b = %h;
    is @a[0], @b[0], 'chained @ = % = list assignment (first)';
    is @a[1], @b[1], 'chained @ = % = list assignment (second)';
    is %h<1>, 2, 'inner hash absorbed the whole comma list';
}

{
    # chained scalar = hash = list
    my ($s, $t, %h);
    $s = %h = 1, 2;
    $t = %h;
    is $s, $t, 'chained $ = % = list assignment';
}

{
    # chained array = array = list
    my (@a, @b);
    @a = @b = 1, 2, 3;
    is @a.elems, 3, 'inner array absorbed the whole comma list';
    is @b.elems, 3, '... and the outer array mirrors it';
    is @a[2], 3, '... last element present';
}

{
    # `%h = k => v, k2 => v2` inner chained through an array
    my (@a, %h);
    @a = %h = "a" => 1, "b" => 2;
    is %h<a>, 1, 'inner hash slurped trailing pairs (a)';
    is %h<b>, 2, 'inner hash slurped trailing pairs (b)';
}

{
    # A parenthesized self-referential list assignment must still snapshot the
    # RHS before storing (the statement-level comma-absorption fix must NOT reroute
    # `(%h1 = %h2, %h1)` through a path that clears %h1 before reading it).
    my %h1 = :1a, :2b; my %h2 = :4b, :3c;
    my $r = (%h1 = %h2, %h1);
    is-deeply %h1, %(:1a, :2b, :3c), 'self-referential paren list assignment keeps RHS snapshot';
}
