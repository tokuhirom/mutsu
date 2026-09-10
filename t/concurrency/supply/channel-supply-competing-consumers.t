use Test;

# A `Channel` is a queue, not a broadcast point: `Channel.Supply` is a view onto
# a `.receive` loop, so two taps on it are COMPETING consumers and each sent
# value reaches exactly one of them. A `Supplier` is the genuine broadcaster and
# still fans out to every tap. See GitHub issue #7604.
#
# Which tap wins each value is not specified, so these assert the PARTITION
# (every value delivered exactly once, nothing duplicated) rather than a
# particular split -- the file passes under rakudo too.

plan 6;

sub union(@a, @b) { (flat @a, @b).sort.join(',') }

# 1-2. One `$c.Supply` object, two taps on it.
{
    my $c = Channel.new;
    my (@a, @b);
    my $s = $c.Supply;
    $s.tap: { @a.push($_) }
    $s.tap: { @b.push($_) }
    $c.send($_) for 1..6;
    $c.close;
    sleep 0.5;
    is @a.elems + @b.elems, 6, 'shared Supply: each value delivered once, not broadcast';
    is union(@a, @b), '1,2,3,4,5,6', 'shared Supply: the two taps partition the stream';
}

# 3-4. Two separate `$c.Supply` calls on the same channel.
{
    my $c = Channel.new;
    my (@a, @b);
    $c.Supply.tap: { @a.push($_) }
    $c.Supply.tap: { @b.push($_) }
    $c.send($_) for 1..6;
    $c.close;
    sleep 0.5;
    is @a.elems + @b.elems, 6, 'two Supply calls: each value delivered once';
    is union(@a, @b), '1,2,3,4,5,6', 'two Supply calls: the two taps partition the stream';
}

# 5. A `Supplier` is a real broadcaster: every tap sees every value.
{
    my $sup = Supplier.new;
    my (@a, @b);
    $sup.Supply.tap: { @a.push($_) }
    $sup.Supply.tap: { @b.push($_) }
    $sup.emit($_) for 1..6;
    is "{@a.join(',')}|{@b.join(',')}", '1,2,3,4,5,6|1,2,3,4,5,6',
       'Supplier still broadcasts to every tap';
}

# 6. A single tap still sees the whole stream, in order.
{
    my $c = Channel.new;
    my @one;
    $c.Supply.tap: { @one.push($_) }
    $c.send($_) for 1..6;
    $c.close;
    sleep 0.5;
    is @one.join(','), '1,2,3,4,5,6', 'a single tap still sees the whole stream in order';
}
