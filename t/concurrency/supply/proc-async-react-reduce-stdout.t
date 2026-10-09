use Test;

plan 8;

sub child(Str $code) { Proc::Async.new($*EXECUTABLE, "-e", $code) }

# The exit promise must not end the react before the reduced stdout emits.
{
    my $p = child('say "x"');
    my $out = "";
    react {
        whenever $p.stdout.reduce: &[~] { $out = $_ }
        whenever $p.start { done }
    }
    is $out, "x\n", "reduce over stdout emits before the exit promise ends the react";
}

{
    my $p = child('print "a"; $*OUT.flush; print "b"');
    my $out;
    react {
        whenever $p.stdout.map(*.uc) { $out ~= $_ }
        whenever $p.start { done }
    }
    is $out, "AB", "map over stdout inside react";
}

{
    my $p = child('say "keep"; $*ERR.say("drop")');
    my @got;
    react {
        whenever $p.stdout.grep(/keep/) { @got.push: $_ }
        whenever $p.start { done }
    }
    is @got.join, "keep\n", "grep over stdout inside react";
}

{
    my $p = child('say "q"');
    my $out = "";
    react {
        whenever $p.stdout.map(*.chomp).reduce(&[~]) { $out = $_ }
        whenever $p.start { done }
    }
    is $out, "q", "stacked map + reduce stages";
}

{
    my $p = child('print ""');
    my $seen = False;
    my $v = "unset";
    react {
        whenever $p.stdout.reduce(&[~]) { $seen = True; $v = $_ }
        whenever $p.start { done }
    }
    ok $seen, "reduce over an empty stdout still emits once";
}

# Outside react: derived taps are fed when the process runs.
{
    my $p = child('say "y"');
    my @m;
    $p.stdout.map(*.uc).tap({ @m.push: $_ });
    my @r;
    $p.stdout.reduce(&[~]).tap({ @r.push: $_ });
    await $p.start;
    is @m.join, "Y\n", "map over stdout via tap";
    is @r.join, "y\n", "reduce over stdout via tap";
}

# `done =>` on a stdout tap fires when the stream ends, not at tap time.
{
    my $p = child('say "z"');
    my @log;
    $p.stdout.tap({ @log.push: "v" }, done => { @log.push: "done" });
    @log.push: "tapped";
    await $p.start;
    is @log.join(","), "tapped,v,done", "done fires after the stream ends";
}
