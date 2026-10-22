use Test;

# A closure made inside a named sub that is itself declared inside a closure
# body keeps the variables of the activation that declared the sub. The sub's
# frame is a lexical child of that activation, not of the program scope, so the
# capture must not stop at the sub's frame (ADR-12529 phase 3).
plan 6;

sub mk($n) {
    my $ro = "req$n";
    return -> {
        sub go() { my $k = -> { $ro }; $k }
        go()
    };
}
my $a = mk(1);
my $b = mk(2);
my $ka = $a();
my $kb = $b();
is $ka(), 'req1', 'the closure made in the nested sub sees its own activation';
is $kb(), 'req2', 'a second activation has its own binding';
is mk(3)()(), 'req3', 'also when called straight away';

sub deep($n, $depth) {
    my $ro = "r$n";
    return -> {
        sub go() {
            $depth > 0 ?? ($ro ~ '>' ~ deep($n + 10, $depth - 1)()()) !! $ro
        }
        -> { go() }
    };
}
is deep(1, 0)()(), 'r1', 'a recursive factory: the base case';
is deep(1, 1)()(), 'r1>r11', 'one level of recursion keeps each level\'s variables';
is deep(2, 2)()(), 'r2>r12>r22', 'two levels';
