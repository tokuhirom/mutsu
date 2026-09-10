use Test;

plan 13;

# Basic: the block exits with the expected code.
exits-ok({ exit 4 }, 4, "exits with code 4");

# stdout produced before the exit is still emitted.
exits-ok({ print ""; exit 2 }, 2, "exits with code 2 after output");

# Explicit exit 0.
exits-ok({ exit 0 }, 0, "exits with code 0");

# A bare `exit` defaults to code 0.
exits-ok({ exit }, 0, "bare exit defaults to code 0");

# The expected-exit-code argument defaults to 0 when omitted.
exits-ok({ exit 0 });

# Block form without parentheses.
exits-ok {
    exit 3;
}, 3, "block form exits with code 3";

# High POSIX code.
exits-ok({ exit 127 }, 127, "exits with code 127");

# Return value is True on success, False on failure.
my $ok = exits-ok({ exit 7 }, 7, "returns True on match");
ok $ok, "exits-ok returns True when the code matches";

# Negative cases are marked TODO so their failing TAP lines do not fail the plan,
# while their return value is asserted to be False.
{
    my $bad;
    todo "wrong exit code should not match";
    $bad = exits-ok({ exit 5 }, 4, "wrong code does not match");
    nok $bad, "exits-ok returns False when the code differs";
}

{
    my $bad;
    todo "block that never exits should not match";
    $bad = exits-ok({ 42 }, 0, "no exit does not match even for code 0");
    nok $bad, "exits-ok returns False when the block never exits";
}
