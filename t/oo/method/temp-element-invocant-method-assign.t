use Test;

# `temp` over a method/attribute lvalue whose invocant is an element
# (`temp @a[0].n = v`, `temp %h<k>.n ~= v`) saves the attribute of the object
# the element holds and restores it at scope exit (#16165). Checked against
# rakudo.

plan 9;

class R { has $.n is rw = 1; has $.s is rw = "a" }
my @a = R.new;
my %h = k => R.new;
my $obj = @a[0];

{
    temp @a[0].n = 9;
    is @a[0].n, 9, 'temp @a[0].n = v assigns';
}
is @a[0].n, 1, '...and restores at scope exit';

{
    temp %h<k>.n = 5;
    is %h<k>.n, 5, 'temp %h<k>.n = v assigns';
}
is %h<k>.n, 1, '...and restores';

{
    temp @a[0].s ~= "b";
    is @a[0].s, "ab", 'temp @a[0].s ~= v assigns the combined value';
}
is @a[0].s, "a", '...and restores';

{
    temp @a[0].n = 7;
    is $obj.n, 7, 'the change is seen through every holder of the object';
}
is $obj.n, 1, '...and so is the restore';
ok $obj === @a[0], 'the element still holds the same object';
