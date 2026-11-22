use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# The argument list of a parenthesized call is rakudo's `arglist`,
# `EXPR('e=')`: its items stop before the loose word-logicals (`and`, `or`,
# `xor`, `andthen`, `orelse`, `notandthen`), so one of those directly inside
# `f(...)` / `.m(...)` leaves the `)` unreachable -- a compile-time
# X::Comp::FailGoal, as on rakudo 2026.11 (#18140). Parenthesizing the
# expression, a capture literal `\(...)` and a listop call are unaffected.

plan 14;

my $fail-goal = ｢Unable to parse expression in argument list; couldn't find final ')'｣;

for ｢sub f(*@a) { }; f(1 and 2)｣,
    ｢sub f(*@a) { }; f(1, 2 or 3)｣,
    ｢sub f(*@a) { }; f(1 xor 2)｣,
    ｢sub f(*@a) { }; f(1 andthen 2)｣,
    ｢sub f(*@a) { }; f(1; 2 orelse 3)｣,
    ｢my @a = 1, 2; say @a.first(* > 1 and * < 5)｣,
    ｢say 'a'.contains('a' and 'x')｣,
    ｢sub f(*@a) { }; my $x = f(1 and 2)｣ -> $code {
    is_run $code, %(:out(''), :err{ .contains: $fail-goal }, :status(* != 0)),
        "rejected: $code";
}

sub f(*@a) { @a }
is-deeply f((1 and 2), 3), [2, 3], 'a parenthesized loose infix is one argument';
is-deeply f(not 1), [False], 'loose prefix `not` stays inside the argument';
is-deeply f(1 ?? 2 !! 3, 4 Z 5), [2, 5], 'the conditional and list infixes stay inside';
is-deeply (f 1 and 2), 2, 'a listop call ends its arguments before `and`';
is \(1 and 2).list, (2,), 'a capture literal reads full expressions';

my @a = 1, 2, 3;
is @a.first((2 and 3)), 3, 'a parenthesized loose infix in a method call';
