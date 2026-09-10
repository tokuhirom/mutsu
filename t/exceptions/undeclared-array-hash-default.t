use Test;

# Under `no strict`, an undeclared @-sigil variable auto-declares as an empty
# Array and a %-sigil variable as an empty Hash (raku semantics), so element
# access yields (Any), .end is -1, and .raku renders as [] / {}.
# Regression pins for Language/perl-nutshell.rakudoc doc-diff findings.

plan 14;

{
    no strict;
    is @months[2].gist, '(Any)', 'undeclared @var element access is (Any)';
    is @months.end, -1, 'undeclared @var .end is -1';
    is @months.raku, '[]', 'undeclared @var .raku is []';
    is @months.^name, 'Array', 'undeclared @var is an Array';
    is @array_of_hashes.raku, '[]', 'undeclared @var .raku (second name) is []';
}

{
    no strict;
    is %calories{"apple"}.gist, '(Any)', 'undeclared %var key access is (Any)';
    is %calories<apple>.gist, '(Any)', 'undeclared %var angle-bracket access is (Any)';
    is %calories.raku, '{}', 'undeclared %var .raku is {}';
    is %calories.^name, 'Hash', 'undeclared %var is a Hash';
    is (join ',', %calories{'pear', 'plum'}), ',', 'undeclared %var 2-key slice joins to one comma';
    is (join ',', %calories<pear plum>), ',', 'undeclared %var angle slice joins to one comma';
}

# A bounded range inside a multi-element subscript reads out-of-bounds indices
# as the typed default, even on an empty/short array (mirrors the standalone
# `@a[8..11]` slice). Previously the range sublist was truncated to empty.
{
    my @a;
    is @a[6, 8..11].raku, '(Any, (Any, Any, Any, Any))', 'bounded range in multi-slice on empty array yields Anys';
    is (join ',', @a[6, 8..11]), ',,,,', 'bounded range in multi-slice on empty array joins to four commas';
}

# An unbounded range in a multi-element subscript must not panic (regression).
{
    my @a = 1, 2, 3, 4, 5;
    is @a[1, 3..*].raku, '(2, (4, 5))', 'unbounded range in multi-slice does not panic and slices the tail';
}
