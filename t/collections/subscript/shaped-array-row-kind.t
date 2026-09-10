use Test;

# A shaped array's rows are mutable sub-Arrays. Populating a multi-dim shaped
# array with List rows (e.g. `<a b>`) must store them as Arrays (`[a, b]`), not
# Lists (`(a, b)`), matching raku — so `.raku` and element mutation behave right.

plan 6;

is Array.new(:shape(2,2), <a b>, <c d>).raku,
   'Array.new(:shape(2, 2), ["a", "b"], ["c", "d"])',
   'Array.new with List rows renders rows as Arrays';

is Array.new(:shape(2,2), [1,2], [3,4]).raku,
   'Array.new(:shape(2, 2), [1, 2], [3, 4])',
   'explicit Array rows unaffected';

{
    my @a[2;2] = <a b>, <c d>;
    is @a.raku, 'Array.new(:shape(2, 2), ["a", "b"], ["c", "d"])',
       'shaped assignment stores Array rows';
}

# multi-dim indexing still works
{
    my $a = Array.new(:shape(2,2), <a b>, <c d>);
    is $a[0;1], 'b', 'index [0;1]';
    is $a[1;0], 'c', 'index [1;0]';
}

# 1D shaped array with a flat list is unaffected
{
    my @a[3] = 1, 2, 3;
    is @a.raku, 'Array.new(:shape(3,), [1, 2, 3])', '1D shaped unaffected';
}
