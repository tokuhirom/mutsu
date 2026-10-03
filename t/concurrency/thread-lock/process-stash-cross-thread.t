use Test;

# ADR-11318 / #11318: `PROCESS::` is one stash for the whole process. A write
# to `$PROCESS::OUT` (or a `$*OUT = ...` that lands on the process binding) is
# seen by every thread, including one that was already running.

plan 12;

my class Cap is IO::Handle {
    has @.got;
    submethod TWEAK { self.encoding: 'utf8' }
    method WRITE(IO::Handle:D: Blob:D \data --> Bool:D) { @!got.push: data.decode; True }
}

{
    my $go = Promise.new;
    my $t = start { await $go; say "from thread" };
    my $orig = $PROCESS::OUT;
    my $c = Cap.new;
    $PROCESS::OUT = $c;
    $go.keep;
    await $t;
    $PROCESS::OUT = $orig;
    is-deeply $c.got, ["from thread\n"], 'a running thread sees a $PROCESS::OUT swap';
}

{
    my $go = Promise.new;
    my $t = start { await $go; $*OUT.print: "via \$*OUT\n" };
    my $orig = $PROCESS::OUT;
    my $c = Cap.new;
    $PROCESS::OUT = $c;
    $go.keep;
    await $t;
    $PROCESS::OUT = $orig;
    is-deeply $c.got, ["via \$*OUT\n"], 'a running thread reads the swapped handle through $*OUT';
}

{
    my $go = Promise.new;
    my $done = Promise.new;
    my $c = Cap.new;
    my $t = start { await $go; $PROCESS::OUT = $c; $done.keep };
    my $orig = $PROCESS::OUT;
    $go.keep;
    await $done;
    say "after the thread swapped";
    $PROCESS::OUT = $orig;
    await $t;
    is-deeply $c.got, ["after the thread swapped\n"], 'a swap made on a worker thread is seen by the main thread';
}

{
    my $go = Promise.new;
    my $c = Cap.new;
    my $t = start { await $go; note "to err" };
    my $orig = $PROCESS::ERR;
    $PROCESS::ERR = $c;
    $go.keep;
    await $t;
    $PROCESS::ERR = $orig;
    is-deeply $c.got, ["to err\n"], '$PROCESS::ERR swaps reach a running thread too';
}

{
    my $go = Promise.new;
    my $t = start { await $go; $*PSTASHTEST };
    PROCESS::<$PSTASHTEST> = 42;
    $go.keep;
    is (await $t), 42, 'a PROCESS::<$x> install is seen by a running thread';
}

{
    my $mine = Cap.new;
    my $go = Promise.new;
    my $t = start { my $*OUT = $mine; await $go; say "to mine" };
    my $orig = $PROCESS::OUT;
    my $c = Cap.new;
    $PROCESS::OUT = $c;
    $go.keep;
    await $t;
    $PROCESS::OUT = $orig;
    is-deeply $mine.got, ["to mine\n"], 'a thread\'s own `my $*OUT` still wins over the process value';
    is-deeply $c.got, [], 'and nothing reaches the process handle';
}

{
    my $orig = $PROCESS::OUT;
    my $c = Cap.new;
    my $d = Cap.new;
    $PROCESS::OUT = $c;
    sub inner { say "in inner" }
    { my $*OUT = $d; inner() }
    say "outside";
    $PROCESS::OUT = $orig;
    is-deeply $d.got, ["in inner\n"], 'a caller\'s `my $*OUT` shadows the process value';
    is-deeply $c.got, ["outside\n"], 'the process value is back outside that scope';
}

{
    my $orig = $PROCESS::OUT;
    my $c = Cap.new;
    my $t = Cap.new;
    $PROCESS::OUT = $c;
    sub tmp { temp $*OUT = $t; say "temp" }
    tmp();
    say "after temp";
    $PROCESS::OUT = $orig;
    is-deeply $t.got, ["temp\n"], '`temp $*OUT` over a swapped process value';
    is-deeply $c.got, ["after temp\n"], 'and the swapped value is restored after it';
}

{
    my $orig = $PROCESS::OUT;
    my $c = Cap.new;
    sub swap { $PROCESS::OUT = $c }
    swap();
    say "after swap";
    $PROCESS::OUT = $orig;
    is-deeply $c.got, ["after swap\n"], 'a swap made in a callee outlives its frame';
}
