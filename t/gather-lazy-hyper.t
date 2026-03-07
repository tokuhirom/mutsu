use Test;

plan 4;

{
    my $counter = 0;
    my $test := gather { for 1 .. 5 { $counter++; take $_ } };
    is($test[0], 1, 'gather indexing only pulls the first element');
    is($counter, 1, 'gather remains lazy until more elements are requested');
}

{
    my $counter = 0;
    my $test := hyper gather { for 1 .. 5 { $counter++; take $_ } };
    is($test.sort.values, <1 2 3 4 5>, 'hyper gather materializes all values');
    is($counter, 5, 'hyper gather evaluates the whole source');
}
