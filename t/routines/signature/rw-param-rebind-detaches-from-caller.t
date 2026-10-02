use v6;
use Test;

plan 11;

# Rebinding an `is rw` parameter with `:=` rebinds the NAME only: from then on
# the body no longer refers to the caller's variable, so the caller keeps the
# value it had at the rebind (#10361). A `for` loop in the body (or any other
# construct that sends the call down the full call path) must not change that.

{
    sub a($p is rw) { for 1 { }; $p := $p<a>; 0 }
    my $e = {};
    a($e);
    is $e.raku, '${}', 'rebind to an element after a for loop leaves the caller alone';
}

{
    sub a($p is rw) { if 1 { $p := $p<a>; }; 0 }
    my $e = {};
    a($e);
    is $e.raku, '${}', 'rebind inside an if block leaves the caller alone';
}

{
    sub a($p is rw) { for 1 { $p := $p<a> }; 0 }
    my $e = {};
    a($e);
    is $e.raku, '${}', 'rebind inside the for loop body leaves the caller alone';
}

{
    sub a($p is rw) { for 1 { }; my $x = 5; $p := $x; $x = 7; $p }
    my $e = 1;
    is a($e), 7, 'the rebound name sees the new container';
    is $e, 1, '... while the caller keeps its value';
}

{
    sub a($p is rw) { for 1 { }; $p = 2; my $x = 3; $p := $x; $p = 4; 0 }
    my $e = 1;
    a($e);
    is $e, 2, 'a write before the rebind still reaches the caller';
}

{
    sub b($p is rw, $c) { for 1 { }; if $c { my $x = 3; $p := $x }; $p = 9; 0 }
    my $e1 = 1;
    b($e1, False);
    is $e1, 9, 'a rebind that does not run leaves the writeback intact';
    my $e2 = 1;
    b($e2, True);
    is $e2, 1, 'a rebind that runs detaches the param';
}

{
    sub r($p is rw, $n) { for 1 { }; if $n { r($p, $n - 1); my $y; $p := $y }; $p ~= "x"; 0 }
    my $e = "";
    r($e, 2);
    is $e, 'x', 'each recursion level tracks its own rebind';
}

{
    class C { method m($p is rw) { for 1 { }; $p := $p<a>; 0 } }
    my $e = {};
    C.m($e);
    is $e.raku, '${}', 'method rw param rebind leaves the caller alone';
}

{
    my &cl = -> $p is rw { for 1 { }; $p = 3; my $z; $p := $z; $p = 4 };
    my $e = 1;
    cl($e);
    is $e, 3, 'pointy-block rw param rebind detaches after the earlier write';
}
