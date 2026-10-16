use Test;

# ADR-0072 runs a resuming CATCH/CONTROL handler inline at the throw (or
# `warn`) site, but it belongs to the block that installed it: a lexical it
# writes is that block's, never a same-named lexical of the routine that threw
# or of a frame in between (ADR-12529 §3, slice 22). The handler writes the
# installing block's live slots; it no longer relies on each frame's return
# merge to carry a by-name env write back up.

plan 10;

class X::Neg is Exception { method message() { "neg" } }

# The routine that throws has its own `$caught`/`$m`.
sub guard() { my $caught = 'callee'; X::Neg.new.throw; $caught }
sub warner() { my $m = 'callee'; warn "W"; $m }
sub mid() { my $m = 'mid'; my $r = warner(); "$m/$r" }

{
    my $caught = False;
    CATCH { default { $caught = True; .resume } }
    my $r = guard();
    ok $caught, 'a resuming CATCH writes its own block lexical';
    is $r, 'callee', 'and leaves the throwing sub\'s same-named lexical alone';
}

{
    my $m = '';
    CONTROL { when CX::Warn { $m = .message; .resume } }
    my $r = warner();
    is $m, 'W', 'a resuming CONTROL writes its own block lexical';
    is $r, 'callee', 'and leaves the warning sub\'s same-named lexical alone';
}

{
    my $m = '';
    CONTROL { when CX::Warn { $m = .message; .resume } }
    my $r = mid();
    is $m, 'W', 'through a frame in between with the same name';
    is $r, 'mid/callee', 'whose lexical and the raise site\'s are both untouched';
}

# A handler's by-name read of its own lexical sees the latest value, also
# after an earlier run wrote only the slot.
{
    my $count = 0;
    sub ct-bad { die "ct" }
    for 1..3 { ct-bad(); CATCH { default { $count++ } } }
    is $count, 3, 'a non-resuming handler accumulates across calls';
}

{
    my $count = 0;
    sub cw { warn "cw" }
    for 1..3 { cw(); CONTROL { when CX::Warn { $count++; .resume } } }
    is $count, 3, 'a resuming CONTROL handler accumulates across calls';
}

# The handler still runs in the dynamic scope of the throw.
{
    my $seen;
    sub dy { my $*WHERE = "inner"; warn "d" }
    {
        my $*WHERE = "outer";
        dy();
        CONTROL { when CX::Warn { $seen = $*WHERE; .resume } }
    }
    is $seen, 'inner', 'a CONTROL handler sees the dynamics of the warning sub';
}

# A write to a lexical from further out still reaches it.
{
    my $outer = 0;
    my &run = -> &b { b() };
    run({
        CONTROL { when CX::Warn { $outer = 42; .resume } }
        warner();
    });
    is $outer, 42, 'a handler write to an enclosing scope\'s lexical survives';
}
