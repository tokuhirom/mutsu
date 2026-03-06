use Test;

plan 2;

{
    my @a = (1..Inf);
    @a[1..10002] = @a[9..10010];
    is ~@a[0, 1, 2], '1 10 11', 'range slice assignment on large range';
}

{
    my @a = (1..Inf);
    @a[2..4] = (99, 100, 101);
    is ~@a[0..5], '1 2 99 100 101 6', 'range slice assignment with finite RHS';
}
