use Test;

# A leading `{ ... }` at statement position is ambiguous: it can start a bare
# block (a standalone statement) or be the left operand of a following custom
# infix operator whose first parameter is a `&closure`. Raku resolves this by
# looking ahead past the block's `}` for a token that cannot itself start a
# new statement — a *declared* infix operator. mutsu's statement parser used
# to commit `{ ... }` to a bare-block statement unconditionally, so the
# operator was parsed as a bogus new statement instead (`Undeclared routine`).
# Found via the `PSpec` distribution's `xxx` helper:
# `sub infix:<xxx>(&closure, Int $num) { $num times &closure }`, called as
# `{ $value--; } xxx 25;`.

plan 5;

{
    sub infix:<my-zork>(&closure, Int $num) {
        for ^$num { closure() }
    }
    my $value = 0;
    { $value++; } my-zork 5;
    is $value, 5, 'a bare block followed by a declared word infix is its left operand';
}

{
    sub infix:<my-xxx>(&closure, Int $num) { closure() for ^$num }
    my $value = 0;
    { $value--; } my-xxx 3;
    is $value, -3, 'closure-consuming custom infix runs the block the declared number of times';
}

{
    my @seen;
    sub infix:<⚡>(&closure, Int $num) { @seen.push($num); closure() }
    { @seen.push('block') } ⚡ 9;
    is @seen, [9, 'block'], 'a bare block followed by a declared symbol infix is its left operand';
}

# The common case — a bare block followed on its own line by an unrelated
# statement — must still run as two independent statements.
{
    my $x = 0;
    {
        $x++;
    }
    is $x, 1, 'a bare block on its own line is unaffected';
}

# An undeclared word after a bare block is not treated as a custom infix: the
# block still runs as its own statement and the following call runs as usual.
{
    my @log;
    sub logit($n) { @log.push($n) }
    { @log.push('b') } logit(1);
    is @log, ['b', 1], 'a bare block followed by an ordinary undeclared-infix call stays two statements';
}

done-testing;
