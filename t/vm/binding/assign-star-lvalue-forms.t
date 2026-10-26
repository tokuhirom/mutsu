use Test;

plan 8;

# A parenthesised list lvalue with one real target and a `*` takes that
# target's element by position. The statement form and the expression form
# share one lowering (the dead statement-only twin was deleted).
my ($a, $b, @c, @arr, %hh);
my @l = 1, 2, 3, 4;

(*, $a) = @l;
is $a, 2, '(*, $a) = @l takes the second element';
($b, *) = @l;
is $b, 1, '($b, *) = @l takes the first element';
(@c, *) = @l;
is @c.raku, '[1, 2, 3, 4]', '(@c, *) = @l: a leading array takes the whole list';
(*, @c) = @l;
is @c.raku, '[2, 3, 4]', '(*, @c) = @l: a trailing array skips the leading elements';
@arr = 0, 0;
(*, @arr[1]) = @l;
is @arr.raku, '[0, 2]', '(*, @arr[1]) = @l assigns the element';
(*, %hh<k>) = @l;
is %hh.raku, '{:k(2)}', '(*, %hh<k>) = @l assigns the keyed element';

# `Z=` and a bracketed meta-assignment over a subscript.
my @z = 1, 2, 3;
@z Z= 9, 8;
is @z.raku, '[9, 8, 3]', '@z Z= 9, 8 assigns element-wise';
my %g = a => 1;
%g<a> [R//]= 5;
is %g.raku, '{:a(5)}', '%g<a> [R//]= 5 stores through the subscript';
