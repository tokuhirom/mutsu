use Test;

plan 10;

# `for %h<k>.values { $_ *= 2 }` aliases the array element's values and writes
# the mutations back into %h<k>.
{
    my %h = a => [1, 2, 3];
    for %h<a>.values { $_ *= 2 }
    is-deeply %h<a>, [2, 4, 6], 'hash element .values writeback';
}

# Array element source.
{
    my @a = [10, 20], [30, 40];
    for @a[0].values { $_ += 1 }
    is-deeply @a[0], [11, 21], 'array element .values writeback';
    is-deeply @a[1], [30, 40], 'sibling element untouched';
}

# Nested element source.
{
    my %h = a => { b => [1, 2, 3] };
    for %h<a><b>.values { $_ *= 10 }
    is-deeply %h<a><b>, [10, 20, 30], 'nested element .values writeback';
}

# `last` mid-loop writes back the partial mutations.
{
    my %h = a => [1, 2, 3, 4];
    for %h<a>.values { last if $_ == 3; $_ = 0 }
    is-deeply %h<a>, [0, 0, 3, 4], 'partial writeback on last';
}

# Pointy `is rw` named param.
{
    my %h = a => [1, 2, 3];
    for %h<a>.values -> $v is rw { $v += 100 }
    is-deeply %h<a>, [101, 102, 103], 'pointy is rw element writeback';
}

# Read-only loop does not mutate the element.
{
    my %h = a => [1, 2, 3];
    my $sum = 0;
    for %h<a>.values { $sum += $_ }
    is $sum, 6, 'read-only loop sums elements';
    is-deeply %h<a>, [1, 2, 3], 'read-only loop leaves element unchanged';
}

# Element source nested inside an outer loop.
{
    my %m = a => [1, 2, 3], b => [4, 5, 6];
    for <a b> -> $k {
        for %m{$k}.values { $_ *= 2 }
    }
    is-deeply %m<a>, [2, 4, 6], 'outer-loop element writeback (a)';
    is-deeply %m<b>, [8, 10, 12], 'outer-loop element writeback (b)';
}
