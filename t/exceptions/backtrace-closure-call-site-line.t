use Test;
plan 5;

# A sub frame is shown at the line it was executing when it called the next
# frame. A closure called from the sub must not take that line from a call-site
# line an earlier call left pending (`try { g }` recorded the line of `g`'s call).
# Expected lines are rakudo 2026.09's; only the user-declared frames are compared.

sub lines-of($ex, *@names) {
    $ex.backtrace.grep({ .subname eq @names.any }).map(*.line).List
}

sub g {
    my $c = { die "y" };
    $c();
}
try { g };
is-deeply lines-of($!, 'g'), (15,), 'a sub that calls a stored closure';

sub h {
    my $c = {
        die "z" };

    1;
    $c();
}
try { h };
is-deeply lines-of($!, 'h'), (25,), 'the call line of a closure on a later line';

sub c {
    my $x = 1;
    { { die "bad" }() }()
}
try { c };
is-deeply lines-of($!, 'c'), (32,), 'nested immediately-invoked blocks';

sub d { h }
try { d };
is-deeply lines-of($!, 'h', 'd'), (25, 37), 'frames above a closure-calling sub keep their call lines';

sub plain { die "x" }
try { plain };
is-deeply lines-of($!, 'plain'), (41,), 'a direct die is unchanged';
