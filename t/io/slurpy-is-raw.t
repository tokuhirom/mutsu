use Test;

plan 4;

{
    my @a = 1..3;
    @a[*-1] = 'z';
    is @a[2], 'z', 'WhateverCode index assignment updates last element';
}

{
    my @test = 1..5;
    my $test = 42;

    lives-ok {
        my sub should_work ( *@list is raw ) {
            @list[0]   = 'hi';
            @list[*-1] = 'ho';
        }
        should_work(@test, $test);
    }, "slurpy 'is raw' accepts writable arguments";

    is @test[0], 'hi', 'slurpy raw writeback updates array source';
    is $test, 'ho', 'slurpy raw writeback updates scalar source';
}
