use Test;

# An unhandled `warn` prints through the dynamic `$*ERR`, as Rakudo's default
# handler does, so a rebound `$*ERR` captures the warning (silently, Trap).

plan 6;

my class Capture {
    has @.text;
    method print(*@a) { @!text.push: @a.join; True }
}

{
    my $cap = Capture.new;
    {
        my $*ERR = $cap;
        warn "knock knock";
        note "noted";
    }
    my $all = $cap.text.join;
    ok $all.contains('knock knock'), 'warn goes to a rebound $*ERR';
    ok $all.contains('in block'), 'the warning keeps its location';
    ok $all.index('knock knock') < $all.index('noted'), 'warn and note interleave in order';
}

{
    my $cap = Capture.new;
    sub inner { warn "from a sub" }
    {
        my $*ERR = $cap;
        inner();
    }
    ok $cap.text.join.contains('from a sub'), 'the dynamic $*ERR is seen from a called sub';
}

{
    my $cap = Capture.new;
    {
        my $*ERR = $cap;
        quietly warn "hushed";
    }
    nok $cap.text.join.contains('hushed'), 'quietly still suppresses the warning';
}

{
    my $cap = Capture.new;
    {
        my $*ERR = $cap;
        CONTROL { when CX::Warn { .resume } }
        warn "handled";
    }
    nok $cap.text.join.contains('handled'), 'a CONTROL handler still takes the warning first';
}
