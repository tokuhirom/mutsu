use Test;

plan 4;

# Angle bracket word list starting with a bare hyphen
my @a = <- a - b - c ->;
is @a.elems, 7, '<- a - b - c -> has 7 elements';
is @a[0], '-', 'first element is hyphen';
is @a[3], 'b', 'fourth element is b';
is @a[6], '-', 'last element is hyphen';
