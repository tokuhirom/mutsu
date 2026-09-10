use Test;

# A Whatever- or Inf-ended Range slice is clipped to the target array's
# current length instead of being expanded to the lazy-range prefix. See
# GitHub issue #7674.

plan 10;

{
    my @d = 1..5;
    is (@d[1..*] = 9).raku, '(9, Any, Any, Any)',
        '1..* reports the values assigned through the clipped slice';
    is @d.raku, '[1, 9, Any, Any, Any]',
        '1..* stores only through the end of the existing array';
}

{
    my @d = 1..5;
    is (@d[1..^Inf] = 9).raku, '(9, Any, Any, Any)',
        '1..^Inf is clipped to the array length';
}

{
    my @d = 1..5;
    is (@d[1^..Inf] = 9).raku, '(9, Any, Any)',
        '1^..Inf is clipped after excluding its start';
}

{
    my @d = 1..5;
    is (@d[1^..^Inf] = 9).raku, '(9, Any, Any)',
        '1^..^Inf is clipped with both endpoint rules';
}

{
    my @d = 1..5;
    is (@d[^Inf] = 9).raku, '(9, Any, Any, Any, Any)',
        '^Inf selects the whole existing array';
}

{
    my @d = 1..5;
    is (@d[5..*] = 9).raku, '()',
        'a Whatever-ended slice starting at the end is empty';
    is @d.raku, '[1, 2, 3, 4, 5]',
        'an empty Whatever-ended slice leaves the array unchanged';
}

{
    my @d;
    is (@d[0..*] = 1).raku, '()',
        'an unbounded slice on an empty array is empty';
    is @d.raku, '[]',
        'an unbounded slice does not autovivify an empty array';
}
