use Test;

# A store through a `substr-rw` Proxy splices into the source variable's
# *current* value, and the window follows the length that was stored (#15734).
#
# This is an intentional divergence from rakudo. rakudo's STORE splices into
# the string the Proxy captured at creation (with the original from/length),
# so any change made to the variable after the Proxy was created -- including
# a store through a second Proxy over the same variable -- is silently undone;
# its FETCH reads the live variable with the original length. Neither the
# documentation (Type/Str, method substr-rw) nor roast specifies the store
# side, and roast/S32-str/substr-rw.t only pins that reads are live, which
# holds here too. Expected rakudo output is noted per test.
plan 8;

{
    my $x = "hello";
    my $y := substr-rw($x, 0, 1);
    my $z := substr-rw($x, 4, 1);
    $y = "J";
    is $x, 'Jello', 'a store through the first Proxy';
    $z = "O";
    is $x, 'JellO', 'a second Proxy keeps the first store (rakudo: hellO)';
}

{
    my $s = "abcdef";
    my $r := substr-rw($s, 1, 2);
    $r = "XYZ";
    is $r, 'XYZ', 'a read after a store sees the stored window (rakudo: XY)';
    $r = "Q";
    is $s, 'aQdef', 'the next store replaces the grown window';
    is $r, 'Q', 'and the window shrinks with it (rakudo: Qd)';
}

{
    my $t = "abc";
    my $q := substr-rw($t, 0, 1);
    $t ~= "!";
    $q = "Z";
    is $t, 'Zbc!', 'a write to the variable after the Proxy was made survives a store (rakudo: Zbc)';
}

{
    my $u = "hello";
    my $p := $u.substr-rw(1, 1);
    $u = "HELLO";
    $p = "e";
    is $u, 'HeLLO', 'method form: the store splices into the reassigned value (rakudo: hello)';
    is $p, 'e', 'and reads the window back';
}
