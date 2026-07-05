use Test;

plan 12;

# A mutating hyper postfix (`»++` / `»--`) applied to a subscript/slice used to
# compute the in/decremented values and throw them away, leaving the container
# unchanged. It must write back to each sliced element (autovivifying), and — as
# a post-increment — yield the OLD values.

# --- hash slice ---
{
    my %h;
    %h<a b c>»++;
    is-deeply %h.sort.list, (a => 1, b => 1, c => 1), 'hash-slice »++ autovivifies to 1';
}
{
    my %h = a => 1, b => 2;
    %h<a b>»++;
    is-deeply %h.sort.list, (a => 2, b => 3), 'hash-slice »++ increments existing values';
}
{
    my %h = a => 5, b => 10;
    my @old = %h<a b>»++;
    is-deeply @old, [5, 10], 'hash-slice »++ returns the OLD values (post-increment)';
    is-deeply %h.sort.list, (a => 6, b => 11), '...and the hash is mutated';
}
{
    my %h = a => 3, b => 3;
    %h<a b>»--;
    is-deeply %h.sort.list, (a => 2, b => 2), 'hash-slice »-- decrements';
}

# --- array slice ---
{
    my @a;
    @a[0, 1, 2]»++;
    is-deeply @a, [1, 1, 1], 'array-slice »++ autovivifies';
}
{
    my @a = 1, 2, 3;
    my @old = @a[0, 1]»++;
    is-deeply @old, [1, 2], 'array-slice »++ returns the OLD values';
    is-deeply @a, [2, 3, 3], '...and the array is mutated';
}
{
    my @a = 5, 5;
    @a[0, 1]»--;
    is-deeply @a, [4, 4], 'array-slice »-- decrements';
}
{
    my @a = 1, 2, 3;
    @a[*]»++;
    is-deeply @a, [2, 3, 4], 'whatever-slice »++ increments every element';
}

# --- whole-container and single-element forms are unaffected ---
{
    my @a = 1, 2, 3;
    @a»++;
    is-deeply @a, [2, 3, 4], 'whole-array »++ still works';
}
{
    my %h;
    %h<a>++;
    is-deeply %h, {a => 1}, 'single-element ++ still works';
}
