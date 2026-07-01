use Test;

plan 5;

# A non-integer (string) range hash subscript is a slice over the enumerated
# keys, not a single stringified key.

{
    my %h;
    %h{'x'..'z'} = 1, 2, 3;
    is %h<x>, 1, q{%h{'x'..'z'} = 1,2,3 sets key x};
    is %h<y>, 2, '... key y';
    is %h<z>, 3, '... key z';
    nok %h{'x y z'}:exists, 'no stringified "x y z" key was created';
}

# Read path over a string range.
{
    my %h = a => 1, b => 2, c => 3;
    is ~%h{'a'..'c'}, '1 2 3', q{%h{'a'..'c'} reads the enumerated keys};
}
