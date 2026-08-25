use Test;

# Divergences between mutsu's Proc::Async and Rakudo's, all verified against
# rakudo 2026.06:
#
#   1. an unrecognised NAMED argument to Proc::Async.new must be absorbed
#      (Rakudo's signature is `(*@args where .so, *%_)`), never turned into an
#      argv element;
#   2. a stdout/stderr stream nobody claimed with `.stdout`/`.stderr`/`.Supply`/
#      `bind-std*` inherits the parent's real handle instead of being captured
#      into a pipe nobody drains;
#   3. every `X::Proc::Async::*` exception renders its real message, not its own
#      type name.
#
# Every child here is bounded (echo/sh/cat all exit on their own) and every
# start Promise is awaited, so nothing in this file depends on a sleep, a
# timeout, or a port.

plan 36;

# Return the exception thrown by &code, or Nil.
sub thrown(&code) {
    my $ex;
    try {
        code();
        CATCH { default { $ex = $_ } }
    }
    $ex;
}

# Run $source under $*EXECUTABLE as a child, capturing BOTH of its streams, and
# return them. Capturing both matters: an uncaptured stream would now (rightly)
# be inherited and would scribble over this file's own TAP output.
sub run-child(Str $source) {
    my $proc = Proc::Async.new($*EXECUTABLE, '-e', $source);
    my $out = '';
    my $err = '';
    $proc.stdout.tap: { $out ~= $^a };
    $proc.stderr.tap: { $err ~= $^a };
    await $proc.start;
    ($out, $err);
}

# --- 1. unrecognised named arguments are absorbed, not spawned ---------------

{
    my $p = Proc::Async.new(:r, 'echo', 'Raku');
    is $p.command.elems, 2, 'unrecognised :r does not become an argv element';
    is $p.command[0], 'echo', 'command[0] is still the program';
    my $out = '';
    $p.stdout.tap: { $out ~= $^a };
    await $p.start;
    is $out, "Raku\n", 'process with an unrecognised named arg still runs';
}

{
    my $p = Proc::Async.new('echo', 'a', :some-unknown-flag, :another(42));
    is $p.command.elems, 2, 'several unrecognised nameds are all absorbed';
    my $out = '';
    $p.stdout.tap: { $out ~= $^a };
    await $p.start;
    is $out, "a\n", 'unrecognised nameds do not disturb the argv';
}

{
    # The nameds Proc::Async does understand keep working.
    my $p = Proc::Async.new(:w, 'cat', :enc('utf-8'), :translate-nl);
    is $p.command.elems, 1, ':w/:enc/:translate-nl are all absorbed from argv';
    ok $p.w, ':w is still honoured';
    my $out = '';
    $p.stdout.tap: { $out ~= $^a };
    my $pr = $p.start;
    await $p.write('fed'.encode);
    $p.close-stdin;
    await $pr;
    is $out, 'fed', ':w still opens stdin when other nameds are present';
}

{
    # No positional at all is X::Multi::NoMatch, exactly as in Rakudo, and an
    # absorbed named must not accidentally satisfy the positional requirement.
    my $ex = thrown { Proc::Async.new(:w) };
    ok $ex.defined, 'Proc::Async.new with no command throws';
    is $ex.^name, 'X::Multi::NoMatch', '... with X::Multi::NoMatch';
}

# --- 2. an unclaimed stream is inherited, not swallowed ----------------------

{
    my ($out, $err) = run-child(
        'my $p = Proc::Async.new(\'echo\', \'PASSTHRU-OUT\'); await $p.start;'
    );
    ok $out.contains('PASSTHRU-OUT'),
        'untapped child stdout passes through to the parent stdout';
    is $err, '', '... and nothing leaks onto stderr';
}

{
    my ($out, $err) = run-child(
        'my $p = Proc::Async.new(\'sh\', \'-c\', \'echo PASSTHRU-ERR >&2\'); await $p.start;'
    );
    ok $err.contains('PASSTHRU-ERR'),
        'untapped child stderr passes through to the parent stderr';
    is $out, '', '... and nothing leaks onto stdout';
}

{
    # Both streams unclaimed at once, each landing on its own handle.
    my ($out, $err) = run-child(
        'my $p = Proc::Async.new(\'sh\', \'-c\', \'echo BOTH-OUT; echo BOTH-ERR >&2\'); await $p.start;'
    );
    ok $out.contains('BOTH-OUT'), 'both-unclaimed: stdout reaches parent stdout';
    ok $err.contains('BOTH-ERR'), 'both-unclaimed: stderr reaches parent stderr';
    nok $out.contains('BOTH-ERR'), 'both-unclaimed: streams are not merged';
}

{
    # A CLAIMED stream is captured, so it must NOT also reach the parent's
    # handle; only the tap sees it.
    my ($out, $err) = run-child(
        'my $p = Proc::Async.new(\'echo\', \'CAPTURED\');'
        ~ ' $p.stdout.tap({ print "SEEN:" ~ $_ }); await $p.start;'
    );
    ok $out.contains('SEEN:CAPTURED'), 'a tapped stdout still reaches its tap';
    is $out.comb(/CAPTURED/).elems, 1,
        'a tapped stdout is not ALSO echoed to the parent handle';
    is $err, '', 'tapped-stdout case leaves stderr clean';
}

{
    # Mixed: stdout claimed, stderr not. The claimed one is captured, the
    # unclaimed one is inherited.
    my ($out, $err) = run-child(
        'my $p = Proc::Async.new(\'sh\', \'-c\', \'echo MIX-OUT; echo MIX-ERR >&2\');'
        ~ ' $p.stdout.tap({ print "TAP:" ~ $_ }); await $p.start;'
    );
    ok $out.contains('TAP:MIX-OUT'), 'mixed: claimed stdout goes to the tap';
    ok $err.contains('MIX-ERR'), 'mixed: unclaimed stderr is inherited';
}

{
    # A Supply fetched before .start but tapped after it still receives the
    # output: the capture decision is made by the accessor, not by the tap.
    my $p = Proc::Async.new('echo', 'LATE');
    my $s = $p.stdout;
    my $out = '';
    my $pr = $p.start;
    $s.tap: { $out ~= $^a };
    await $pr;
    is $out, "LATE\n", 'a Supply tapped after .start still gets the output';
}

# --- 3. X::Proc::Async::* messages ------------------------------------------

{
    my $ex = thrown { Proc::Async.new('echo', :w).say(42) };
    is $ex.^name, 'X::Proc::Async::MustBeStarted', 'MustBeStarted is thrown';
    is $ex.message, "Process must be started first before calling 'say'",
        'MustBeStarted.message names the method';
    is $ex.Str, $ex.message, 'MustBeStarted.Str is the message, not the type name';
}

{
    my $ex = thrown {
        my $p = Proc::Async.new('echo');
        $p.stdout;
        $p.stdout(:bin);
    };
    is $ex.^name, 'X::Proc::Async::CharsOrBytes', 'CharsOrBytes is thrown';
    is $ex.message, 'Can only tap one of chars or bytes supply for stdout',
        'CharsOrBytes.message names the handle';
    is $ex.Str, $ex.message, 'CharsOrBytes.Str is the message, not the type name';
}

{
    my $ex = thrown { Proc::Async.new('echo').write(Buf.new(1)) };
    is $ex.message, "Process must be opened for writing with :w to call 'write'",
        'OpenForWriting.message names the method';
}

{
    my $ex = thrown {
        my $p = Proc::Async.new('echo');
        $p.Supply;
        $p.stdout;
    };
    is $ex.^name, 'X::Proc::Async::SupplyOrStd', 'SupplyOrStd is thrown';
    ok $ex.message.starts-with('Using .Supply on a Proc::Async'),
        'SupplyOrStd.message explains the conflict';
}

{
    my $ex = thrown {
        my $p = Proc::Async.new(:w, 'cat');
        $p.bind-stdin('/dev/null'.IO.open);
    };
    is $ex.^name, 'X::Proc::Async::BindOrUse', 'BindOrUse is thrown';
    is $ex.message, 'Cannot both bind stdin to a handle and also use :w',
        'BindOrUse.message names the handle and the conflicting use';
}

{
    # Constructing one by hand must render the same way the thrown one does.
    is X::Proc::Async::MustBeStarted.new(method => 'write').message,
        "Process must be started first before calling 'write'",
        'a hand-built MustBeStarted formats its own message';
    is X::Proc::Async::AlreadyStarted.new.message,
        'Process has already been started',
        'a hand-built AlreadyStarted formats its own message';
}

# vim: expandtab shiftwidth=4
