use Test;

plan 12;

# A block that merely *contains* a CATCH/CONTROL phaser is not a `try`. The
# compiler wraps such a body in the same TryCatch region a real `try` uses, but
# only the real one traps: an exception no handler matched must propagate.

{
    my $ran = False;
    my $died = False;
    try {
        { die "boom"; CONTROL { } };
        $ran = True;
        CATCH { default { $died = True } }
    }
    ok $died, 'a block with only a CONTROL phaser does not swallow a die';
    nok $ran, 'execution does not continue past the propagating die';
}

{
    my $died = False;
    try {
        sub inner { die "boom"; CONTROL { } }
        inner();
        CATCH { default { $died = True } }
    }
    ok $died, 'a routine body with only a CONTROL phaser does not swallow a die';
}

{
    my $died = False;
    try {
        { die "boom"; CATCH { when X::Numeric::Overflow { } } };
        CATCH { default { $died = True } }
    }
    ok $died, 'an unmatched CATCH re-throws out of a plain block';
}

# A genuine `try` still traps, with or without a CATCH block.
{
    my $r = try { die "boom"; 42 };
    nok $r.defined, 'a bare try still swallows the exception';
    is $!.message, 'boom', 'and leaves the exception in $!';
}

{
    my $seen = '';
    my $r = try { die "boom"; CATCH { default { $seen = .message } } };
    is $seen, 'boom', 'a matching CATCH inside a try still runs';
}

# A CONTROL block only *handles* the signal when a `when`/`default` matched.
{
    my $ran = False;
    my $h = '';
    try {
        CONTROL { $h = 'ran' };
        next;
        $ran = True;
    }
    is $h, 'ran', 'an unmatched CONTROL block still runs its body';
    nok $ran, 'the declined signal does not resume the protected body';
    ok $! ~~ X::ControlFlow,
        'a loop-control signal no CONTROL matched is a catchable X::ControlFlow';
}

{
    my $h = '';
    try {
        CONTROL { default { $h = 'handled' } };
        next;
    }
    is $h, 'handled', 'a CONTROL block with a default handles the signal';
    nok $! ~~ X::ControlFlow, 'a handled signal leaves $! alone';
}
