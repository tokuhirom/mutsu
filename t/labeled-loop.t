use Test;
plan 5;

# Labeled loop with next LABEL
my $result = '';
OUTER: for 1..3 -> $i {
    for 1..3 -> $j {
        next OUTER if $j == 2;
        $result = $result ~ "$i.$j ";
    }
}
is $result, '1.1 2.1 3.1 ', 'next LABEL skips to outer loop';

# Labeled loop with last LABEL
my $result2 = '';
OUTER: for 1..3 -> $i {
    for 1..3 -> $j {
        last OUTER if $i == 2 && $j == 1;
        $result2 = $result2 ~ "$i.$j ";
    }
}
is $result2, '1.1 1.2 1.3 ', 'last LABEL breaks outer loop';

# Plain last/next without label still works
my @items = 1, 2, 3, 4, 5;
my $sum = 0;
for @items -> $n {
    next if $n == 3;
    last if $n == 5;
    $sum = $sum + $n;
}
is $sum, 7, 'plain last/next still works';

# Labeled redo from inner loop targets outer loop
{
    my $x;
    my $out = '';
    FOO: for "foo" {
        $out ~= $_;
        BAR: for "bar" {
            $out ~= $_;
            redo FOO unless $x++
        }
    }
    is $out, 'foobarfoobar', 'redo LABEL from inner loop redoes outer loop';
}

# redo LABEL with unless modifier
{
    my $count = 0;
    FOO: for "a" {
        $count++;
        redo FOO unless $count >= 3;
    }
    is $count, 3, 'redo LABEL unless works correctly';
}
