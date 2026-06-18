use Test;

plan 6;

# `grep BLOCK, <Seq>` must flatten the (eager) Seq into the list to grep over,
# the same way `map` and an Array argument already do. `@a.sort` returns a Seq.
{
    my @a = 3, 1, 2;
    is (grep { 1 }, @a.sort).elems, 3, 'grep flattens a Seq from .sort (count)';
    is-deeply (grep { $_ > 1 }, @a.sort).List, (2, 3),
        'grep filters over a flattened Seq';
}

# The classic for-loop form from S04-statements/for.t test 46.
{
    my @array = 1, 2, 3, 4;
    my $output = '';
    for (grep { 1 }, @array.sort) -> $elem {
        $output ~= "$elem,";
    }
    is $output, "1,2,3,4,", 'grep and sort work in a for loop';
}

# A Seq held in a scalar is still flattened by grep (matches Rakudo).
{
    my @a = 1, 2, 3;
    my $s = @a.sort;
    is (grep { $_ > 1 }, $s).elems, 2, 'grep flattens a Seq held in a scalar';
}

# An explicit comma list and an array argument keep working unchanged.
{
    is (grep { $_ %% 2 }, 1, 2, 3, 4).elems, 2, 'grep over an explicit comma list';
    my @a = 1, 2, 3, 4, 5, 6;
    is (grep { $_ %% 2 }, @a).elems, 3, 'grep over an array variable';
}
