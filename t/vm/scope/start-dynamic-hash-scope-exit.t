use Test;

plan 5;

# A `start` inside a block that rebinds `%*M` must not leave that binding
# visible to threads started after the block is left.
sub show { %*M.raku }
my %*M = bla => "ble";
is (await start show), '{:bla("ble")}', 'before the block';
{
    my %*M = bli => "blo";
    is (await start show), '{:bli("blo")}', 'inside the block';
}
is (await start show), '{:bla("ble")}', 'after the block';
is (await start try show), '{:bla("ble")}', 'after the block, under try';
my @*A = 1, 2;
{
    my @*A = 3;
    await start { @*A.push: 4 };
    is-deeply @*A.List, (3, 4), 'a start block still mutates the live dynamic array';
}
