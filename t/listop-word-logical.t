use v6;
use Test;

plan 12;

# The loose word-logicals (and/or/andthen/orelse/xor) are LOOSER than a
# listop's no-paren argument list: `f $x and $y` is `(f $x) and $y`,
# not `f($x and $y)`. Pins the statement-level bare-call argument parse
# (Text::CSV: `defined $f and $cf.add ($f.Str)` with $f eq "" must call add).

{
    my $f = "";
    my $hit = False;
    defined $f and $hit = True;
    ok $hit, 'defined $f and ... runs the RHS when $f is a defined empty string';
}

{
    my $hit = False;
    my @a;
    push @a, 5 and $hit = True;
    ok $hit, 'push @a, 5 and ... runs the RHS';
    is-deeply @a, [5], 'push @a, 5 and ... pushes 5, not the and-expression';
}

{
    my $ran = False;
    my @a;
    push @a, 1, 2 or $ran = True;
    is-deeply @a, [1, 2], 'push @a, 1, 2 or ... pushes both args';
    nok $ran, 'or-RHS not run when push returns truthy';
}

# IO listops: `say 0 or die` is `(say 0) or die` — say returns True.
{
    my $died = False;
    say 0 or $died = True;
    nok $died, 'say 0 or ... does not run the RHS (say returns True)';
}

{
    my $order = "";
    say 1, 2 and $order ~= "rhs";
    is $order, "rhs", 'say 1, 2 and ... runs the RHS after saying both args';
}

{
    my $died = False;
    print "" or $died = True;
    nok $died, 'print or-chain does not run the RHS';
}

{
    my $died = False;
    note "note-ok" or $died = True;
    nok $died, 'note or-chain does not run the RHS';
}

{
    my $hit = False;
    put 0 or $hit = True;
    nok $hit, 'put 0 or ... does not run the RHS';
}

# andthen after an IO listop.
{
    my $next = False;
    say "first" andthen $next = True;
    ok $next, 'say ... andthen runs the RHS';
}

# A tight && stays inside the argument.
{
    my @a;
    push @a, 1 && 2;
    is-deeply @a, [2], 'push @a, 1 && 2 pushes the &&-result (tight operator stays in the arg)';
}
