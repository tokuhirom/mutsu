use Test;

# `whenever $proc { ... }` inside a `react` block taps the process's merged
# output Supply (stdout + stderr). It used to deliver nothing at all: the merged
# Supply had no live producer, and was only served after the child exited by the
# await/result-time replay, which the react drive loop never reaches.
#
# Chunk *boundaries* and the relative order of the two pipes are genuinely racy,
# so nothing here pins either; what is pinned is that every byte arrives, exactly
# once, and that the stream is properly ended.

plan 17;

# 1. The headline case: the bare `whenever $proc` coercion.
{
    my $proc = Proc::Async.new('echo', 'merged-hello');
    my $got = '';
    react {
        whenever $proc { $got ~= $_ }
        whenever $proc.start { }
    }
    is $got, "merged-hello\n", 'whenever $proc in react receives the merged output';
}

# 2b. A Proc::Async merge may be obtained before spawn, but it must be tapped
# before spawn.  The diagnosis is at registration time, including the
# `whenever $proc` coercion path that bypasses `Supply.tap`.
{
    my $proc = Proc::Async.new('true');
    my $p = $proc.start;
    my $err = '';
    try {
        react {
            whenever $proc { }
            whenever $p { }
        }
        CATCH { default { $err = $_ } }
    }
    isa-ok $err, X::Proc::Async::TapBeforeSpawn,
        'whenever a started Proc::Async merge throws TapBeforeSpawn';
    is $err.message,
        'To avoid data races, you must tap merge before running the process',
        'the merged-whenever error names merge';
}

{
    my $proc = Proc::Async.new('true');
    my $s = $proc.Supply;
    my $p = $proc.start;
    my $err = '';
    try {
        $s.tap({ });
        CATCH { default { $err = $_ } }
    }
    isa-ok $err, X::Proc::Async::TapBeforeSpawn,
        'a merged Supply fetched early still rejects a late direct tap';
    is $err.message,
        'To avoid data races, you must tap merge before running the process',
        'the late direct tap error names merge';
    await $p;
}

# 2. The same thing with the coercion written out by hand.
{
    my $proc = Proc::Async.new('echo', 'explicit-supply');
    my $s = $proc.Supply;
    my $got = '';
    react {
        whenever $s { $got ~= $_ }
        whenever $proc.start { }
    }
    is $got, "explicit-supply\n", 'whenever $proc.Supply in react receives the merged output';
}

# 3. Both pipes are merged. Their interleaving is a race between two independent
#    reader threads, so assert on the content, never on the order.
{
    my $proc = Proc::Async.new('sh', '-c', 'echo out1; echo out2; echo err1 >&2');
    my $got = '';
    react {
        whenever $proc { $got ~= $_ }
        whenever $proc.start { }
    }
    is $got.lines.sort.join(','), 'err1,out1,out2', 'stdout and stderr are both merged in';
}

# 4. A process that writes nothing still ends the merged Supply cleanly.
{
    my $proc = Proc::Async.new('true');
    my $emits = 0;
    my $last = 0;
    react {
        whenever $proc { $emits++; LAST { $last++ } }
        whenever $proc.start { }
    }
    is $emits, 0, 'a silent process emits nothing on the merged Supply';
    is $last, 1, 'LAST still fires once when the merged Supply ends with no output';
}

# 5. More output than one read buffer holds: every byte arrives, and it arrives
#    incrementally rather than as one post-mortem lump.
{
    my $proc = Proc::Async.new('sh', '-c', 'i=1; while [ $i -le 2000 ]; do echo "line$i"; i=$((i+1)); done');
    my $chunks = 0;
    my $total = 0;
    my $lines = 0;
    react {
        whenever $proc { $chunks++; $total += .chars; $lines += .comb("\n").elems }
        whenever $proc.start { }
    }
    is $total, 16893, 'the whole of a large merged output arrives';
    is $lines, 2000, 'every line of a large merged output arrives';
    ok $chunks > 1, 'a large merged output is delivered incrementally, not as one lump';
}

# 6. `done` inside the merged `whenever` body ends the react.
{
    my $proc = Proc::Async.new('sh', '-c', 'echo first; sleep 30; echo second');
    my $got = '';
    react {
        whenever $proc { $got ~= $_; done }
        whenever $proc.start { }
    }
    is $got, "first\n", 'done in a merged whenever body ends the react immediately';
    $proc.kill('KILL');
}

# 7. Delivery is once and only once. A plain `.tap` outside react, then `await`,
#    must not see the output twice.
{
    my $proc = Proc::Async.new('echo', 'tapped-once');
    my $got = '';
    $proc.Supply.tap({ $got ~= $_ });
    await $proc.start;
    is $got, "tapped-once\n", 'a merged .tap outside react receives the output exactly once';
}

# 8. ... and neither does a react-driven merged tap whose promise is awaited
#    afterwards: the drive loop already delivered every chunk, so the
#    await-time replay must stand down.
{
    my $proc = Proc::Async.new('echo', 'no-double');
    my $got = '';
    my $p;
    react {
        whenever $proc { $got ~= $_ }
        $p = $proc.start;
        whenever $p { }
    }
    await $p;
    is $got, "no-double\n", 'awaiting after a react-driven merged tap does not redeliver';
}

# 9. The same once-only rule for a per-stream Supply tapped by react.
{
    my $proc = Proc::Async.new('echo', 'per-stream-once');
    my $s = $proc.stdout;
    my $got = '';
    my $p = $proc.start;
    react {
        whenever $s { $got ~= $_ }
        whenever $p { }
    }
    await $p;
    is $got, "per-stream-once\n", 'awaiting after a react-driven .stdout tap does not redeliver';
}

# 10. Merging and the per-stream accessors stay mutually exclusive.
{
    my $proc = Proc::Async.new('echo', 'exclusive');
    my $err = '';
    try {
        react {
            whenever $proc { }
            whenever $proc.stdout { }
            whenever $proc.start { }
        }
        CATCH { default { $err = .message } }
    }
    like $err, /'cannot therefore be used in combination'/,
        '.Supply and .stdout together still die with X::Proc::Async::SupplyOrStd';
}
