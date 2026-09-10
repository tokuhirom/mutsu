use Test;

# A Supply is a broadcast point: tapping one twice gives both taps every value
# that arrives after they tapped (ADR-0074). Channel-backed supplies -- the
# Proc::Async output family above all -- used to hand the whole stream to
# whichever consumer asked first, leaving every later tap empty.
#
# Every row here was measured against rakudo v2026.07 before it was written,
# including the rows that already passed: the live-vs-on-demand distinction is
# what forbids a naive buffer-and-replay fan-out, so the rows pinning it are as
# load-bearing as the rows pinning the fix.

plan 18;

# --- Proc::Async output: the family that diverged -------------------------

# stdout, tapped twice inside one react block.
{
    my $proc = Proc::Async.new("echo", "two");
    my $s = $proc.stdout;
    my ($x, $y) = ('', '');
    react {
        whenever $s { $x ~= $_ };
        whenever $s { $y ~= $_ };
        whenever $proc.start { };
    }
    is $x, "two\n", 'first whenever on .stdout gets the output';
    is $y, "two\n", 'second whenever on the same .stdout gets it too';
}

# The merged .Supply, tapped twice.
{
    my $proc = Proc::Async.new("echo", "two");
    my $s = $proc.Supply;
    my ($x, $y) = ('', '');
    react {
        whenever $s { $x ~= $_ };
        whenever $s { $y ~= $_ };
        whenever $proc.start { };
    }
    is $x, "two\n", 'first whenever on the merged .Supply gets the output';
    is $y, "two\n", 'second whenever on the merged .Supply gets it too';
}

# stderr, tapped twice.
{
    my $proc = Proc::Async.new($*EXECUTABLE, "-e", 'note "err"');
    my $s = $proc.stderr;
    my ($x, $y) = ('', '');
    react {
        whenever $s { $x ~= $_ };
        whenever $s { $y ~= $_ };
        whenever $proc.start { };
    }
    is $x, "err\n", 'first whenever on .stderr gets the output';
    is $y, "err\n", 'second whenever on .stderr gets it too';
}

# Two separate .stdout accessor calls, one whenever each: same underlying
# stream, so both must see everything.
{
    my $proc = Proc::Async.new("echo", "two");
    my ($x, $y) = ('', '');
    react {
        whenever $proc.stdout { $x ~= $_ };
        whenever $proc.stdout { $y ~= $_ };
        whenever $proc.start { };
    }
    is "$x|$y", "two\n|two\n", 'two separate .stdout calls both stream';
}

# A derived .lines supply, tapped twice.
{
    my $proc = Proc::Async.new("echo", "two");
    my $s = $proc.stdout.lines;
    my ($x, $y) = ('', '');
    react {
        whenever $s { $x ~= $_ };
        whenever $s { $y ~= $_ };
        whenever $proc.start { };
    }
    is "$x|$y", "two|two", '.stdout.lines fans out to both taps';
}

# Plain .tap twice, outside react: the act-loop pump served only the first
# registered tap callback.
{
    my $proc = Proc::Async.new("echo", "two");
    my $s = $proc.stdout;
    my ($x, $y) = ('', '');
    $s.tap({ $x ~= $_ });
    $s.tap({ $y ~= $_ });
    await $proc.start;
    is "$x|$y", "two\n|two\n", '.tap twice on .stdout feeds both taps';
}

# --- Live semantics: a late tap must NOT be replayed to ------------------

# Proc::Async output is live. A tap added after the process finished gets
# nothing -- a buffer-and-replay fan-out would wrongly hand it the output.
{
    my $proc = Proc::Async.new("echo", "two");
    my $s = $proc.stdout;
    my ($x, $y) = ('', '');
    $s.tap({ $x ~= $_ });
    await $proc.start;
    $s.tap({ $y ~= $_ });
    sleep 0.2;
    is $x, "two\n", 'the tap that was present got the output';
    is $y, '', 'a tap added after the process exited gets nothing (live)';
}

# A Supplier-backed Supply is live in the same way.
{
    my $sup = Supplier.new;
    my $s = $sup.Supply;
    my ($x, $y) = ('', '');
    $s.tap({ $x ~= $_ });
    $sup.emit("a");
    $s.tap({ $y ~= $_ });
    $sup.emit("b");
    $sup.done;
    is "$x|$y", "ab|b", 'a late tap on a live Supplier sees only later values';
}

# Two taps registered up front both see everything.
{
    my $sup = Supplier.new;
    my $s = $sup.Supply;
    my ($x, $y) = ('', '');
    $s.tap({ $x ~= $_ });
    $s.tap({ $y ~= $_ });
    $sup.emit("a");
    $sup.emit("b");
    $sup.done;
    is "$x|$y", "ab|ab", 'two taps on a live Supplier both see every value';
}

# --- On-demand semantics: the producer runs once per tap ------------------

{
    my $runs = 0;
    my $s = supply { $runs++; emit 1; emit 2; done; };
    my ($x, $y) = ('', '');
    $s.tap({ $x ~= $_ });
    $s.tap({ $y ~= $_ });
    is "$x|$y|$runs", "12|12|2",
        'an on-demand supply block runs its body once per tap';
}

{
    my $s = Supply.from-list(1, 2, 3);
    my ($x, $y) = ('', '');
    $s.tap({ $x ~= $_ });
    $s.tap({ $y ~= $_ });
    is "$x|$y", "123|123", 'Supply.from-list feeds both taps';
}

# --- .Channel still drains the stream once -------------------------------

{
    my $s = Supply.from-list(1, 2, 3);
    my $c = $s.Channel;
    my @got;
    loop {
        my $v = $c.receive;
        @got.push($v);
        CATCH { default { last } }
    }
    is @got.join(','), '1,2,3', '.Channel on a Supply receives every value';
}

# --- A socket connection's read Supply does NOT fan out ------------------

# Measured against rakudo v2026.07: tapping an IO::Socket::Async connection's
# .Supply twice does not give both taps the bytes -- the taps compete and one
# wins (which one varies between runs). So the invariant to pin is "exactly
# one tap sees it", not "both do". Never hardcode a port here: listen on 0 and
# read the assigned port back off the tap.
{
    my ($x, $y) = ('', '');
    my $got = Promise.new;
    my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
    my $tap = $listener.tap(-> $conn {
        my $s = $conn.Supply;
        $s.tap({ $x ~= $_; $got.keep(1) unless $got });
        $s.tap({ $y ~= $_ });
    });
    my $port = $tap.socket-port.result;
    my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
    await $client.print("hello");
    await Promise.anyof($got, Promise.in(5));
    sleep 0.3;
    is ($x.chars > 0) + ($y.chars > 0), 1,
        'exactly one tap of a socket read Supply receives the bytes';
    ok ($x ~ $y).starts-with('hell'),
        'the winning socket tap got the data';
    $client.close;
    $tap.close;
}
