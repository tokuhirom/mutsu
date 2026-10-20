use Test;

# An element that `=`-shares a container (`@aoa[0] = @row`, `@a.push(@b)`)
# holds the source's cell through the itemized word, and a later non-bind
# reassignment of the element replaces it rather than writing through. The
# word itself is the mark (ADR-0097): the former name-keyed
# `__mutsu_elem_share::<name>` table missed `.push` (so `@a[0] = 42` wrote 42
# into `@b`) and went stale under a later `:=` bind of the same element.

plan 10;

{
    my @b = 1, 2;
    my @a;
    @a.push(@b);
    @a[0].push(3);
    is-deeply @b, [1, 2, 3], 'a pushed array is shared with its element';
    @a[0] = 42;
    is-deeply @b, [1, 2, 3], 'reassigning the element leaves the pushed array alone';
    is-deeply @a, [42], 'and replaces the element';
}

{
    my @r = 3, 4;
    my @aoa;
    @aoa[0] = @r;
    @aoa[0] = 9;
    is-deeply @r, [3, 4], 'reassigning an =-shared element leaves the source alone';
    is-deeply @aoa, [9], 'and replaces the element';
}

{
    my @r = 1;
    my @x;
    @x[0] = @r;
    my $y = 5;
    @x[0] := $y;
    @x[0] = 6;
    is $y, 6, 'a :=-bound element that was =-shared before writes through';
    is-deeply @r, [1], 'and the earlier share source is untouched';
}

{
    my @r = 1;
    my %h;
    %h<k> = @r;
    %h<k>.push(2);
    is-deeply @r, [1, 2], 'a hash value shares its source array';
    %h<k> = 0;
    is-deeply @r, [1, 2], 'reassigning the hash value leaves the source alone';
    is %h<k>, 0, 'and replaces the value';
}
