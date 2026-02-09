use Test;
plan 4;

my $x = 10;
my $add = -> $n { return $n + $x; };
is $add(5), 15, 'lambda captures outer variable';

my $sum = 0;
for 1..3 -> $i {
    $sum = $sum + $i;
}
is $sum, 6, 'for loop accumulation works';

sub outer($x) {
    sub inner($y) {
        return $x + $y;
    }
    return inner(10);
}
is outer(5), 15, 'nested sub captures outer param';

sub multiplier($factor) {
    return -> $n { return $n * $factor; };
}
my $double = multiplier(2);
is $double(21), 42, 'returned lambda captures enclosing variable';
