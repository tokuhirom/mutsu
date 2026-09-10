use Test;

plan 6;

# `CONTROL { when CX::Warn { ...; .resume } }` is resume-safe: a warn raised
# inside a sub called from the guarded block must resume INSIDE the sub
# (cross-frame), not unwind — and never rewind to an earlier statement
# (the old cross-frame resume_ip corruption caused an infinite loop here).

{
    my $warn-msg = "";
    my $after-warn = False;
    my $after-call = False;
    {
        CONTROL {
            when CX::Warn {
                $warn-msg = .message;
                .resume
            }
        }
        my sub warner() { warn "boom"; $after-warn = True }
        warner();
        $after-call = True;
    }
    is $warn-msg, "boom", 'when CX::Warn handler saw the message';
    ok $after-warn, 'execution resumed inside the sub after the warn';
    ok $after-call, 'execution continued after the call';
}

# Direct warn in the guarded block itself.
{
    my $caught = "";
    my $after = False;
    {
        CONTROL { when CX::Warn { $caught = .message; .resume } }
        warn "direct";
        $after = True;
    }
    is $caught, "direct", 'direct warn caught';
    ok $after, 'direct warn resumed';
}

# A default-arm CONTROL stays resume-safe too.
{
    my $n = 0;
    {
        CONTROL { default { $n++; .resume } }
        my sub w2() { warn "a"; warn "b" }
        w2();
    }
    is $n, 2, 'both warns handled and resumed';
}
