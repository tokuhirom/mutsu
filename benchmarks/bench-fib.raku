# Recursive fibonacci - tests function call overhead and recursion
sub fib(Int $n --> Int) {
    if $n <= 1 {
        return $n;
    }
    return fib($n - 1) + fib($n - 2);
}

my $result = 0;
for 1..25 -> $i {
    $result = fib($i);
}
say "fib(25) = $result";
