use Test;

{
    my $value;
    given ($value = 'abc') { $_ = 'xyz' }
    is $value, 'xyz', 'given topicalizes a scalar assignment as a writable container';
}

{
    my $value = 'a';
    given ($value ~= 'b') { $_ = 'xyz' }
    is $value, 'xyz', 'a compound scalar assignment keeps the writable topic';
}

{
    my @values;
    dies-ok { given (@values = 1, 2) { $_ = 4 } },
        'an array assignment does not make the topic writable';
}

{
    my $value;
    dies-ok { given ($value := 'abc') { $_ = 'xyz' } },
        'binding a scalar to an immutable value keeps the topic read-only';
}

done-testing;
