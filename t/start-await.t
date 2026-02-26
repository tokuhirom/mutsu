use Test;

plan 8;

# Basic start/await
{
    my $p = start { 42 };
    is await($p), 42, 'start/await returns block result';
}

# start with computation
{
    my $p = start { 10 + 20 };
    is await($p), 30, 'start/await with computation';
}

# Multiple starts with await
{
    my @promises = do for ^3 { start { 100 } };
    is @promises.elems, 3, 'do for with start creates array of promises';
    my @results = await @promises;
    is @results.elems, 3, 'await array returns correct number of results';
    is @results[0], 100, 'await array first element correct';
}

# do for collects values
{
    my @vals = do for ^4 { $_ * 2 };
    is @vals.join(','), '0,2,4,6', 'do for collects iteration results';
}

# await accepts promise sequence produced by xx
{
    my @results = await start { 10 } xx 3;
    is @results.elems, 3, 'await with xx repeats start calls';
    is @results[0], 10, 'await with xx returns awaited results';
}
