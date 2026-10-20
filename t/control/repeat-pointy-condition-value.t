use Test;
plan 8;

my @while;
my $i = 0;
repeat while $i < 3 -> $value {
    @while.push: $value;
    $i++;
}
is-deeply @while, [Mu, True, True], 'while starts with Mu and supplies later conditions';

my @until;
my $j = 0;
repeat until $j >= 3 -> $value {
    @until.push: $value;
    $j++;
}
is-deeply @until, [Mu, False, False], 'until supplies the condition before negation';

my @numbers;
my $n = 0;
my $calls = 0;
sub remaining() { $calls++; 3 - $n }
repeat while remaining() -> $value {
    @numbers.push: $value;
    $n++;
}
is-deeply @numbers, [Mu, 2, 1], 'a non-Bool while condition keeps its actual value';
is $calls, 3, 'the condition is evaluated once after each pass';

my @strings;
my $s = 0;
repeat until ($s >= 3 ?? 'done' !! '') -> $value {
    @strings.push: $value;
    $s++;
}
is-deeply @strings, [Mu, '', ''], 'until keeps a false string condition';

my @zero;
my $z = 0;
repeat until ($z >= 2 ?? 42 !! 0) -> $value {
    @zero.push: $value;
    $z++;
}
is-deeply @zero, [Mu, 0], 'until keeps a false numeric condition';

my @next;
my $k = 0;
repeat while 3 - $k -> $value {
    @next.push: $value;
    $k++;
    next;
}
is-deeply @next, [Mu, 2, 1], 'next still evaluates and supplies the next condition';

my @redo;
my $r = 0;
my $passes = 0;
repeat while 2 - $r -> $value {
    @redo.push: $value;
    redo if ++$passes == 1;
    $r++;
}
is-deeply @redo, [Mu, Mu, 1], 'redo retains the value from the same invocation';
