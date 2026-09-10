use Test;
plan 23;

# Basic construction and start
my $p = Proc::Async.new("echo", "hi");
is elems($p.command), 2, 'command stored';
ok !$p.started, 'not started';
my $promise = $p.start;
ok $p.started, 'started';
is $promise.result, 0, 'start returns exit code';

# stdout tap receives output with newlines preserved
{
    my $p2 = Proc::Async.new("echo", "hello world");
    my $out = '';
    $p2.stdout.tap: { $out ~= $^a };
    my $pr = $p2.start;
    await $pr;
    is $out, "hello world\n", 'stdout tap preserves newlines';
}

# stderr tap receives error output
{
    my $p3 = Proc::Async.new("sh", "-c", "echo error-msg >&2");
    my $err = '';
    $p3.stderr.tap: { $err ~= $^a };
    my $pr = $p3.start;
    await $pr;
    is $err, "error-msg\n", 'stderr tap receives error output';
}

# .kill terminates a long-running process
{
    my $p4 = Proc::Async.new("sleep", "60");
    my $pr = $p4.start;
    $p4.kill;
    my $result = $pr.result;
    ok $result.exitcode != 0 || $result.signal != 0, '.kill terminates process';
}

# Proc instance from result
{
    my $p5 = Proc::Async.new("echo", "test");
    my $pr = $p5.start;
    my $proc = $pr.result;
    is $proc.exitcode, 0, 'Proc.exitcode is 0';
    is $proc.signal, 0, 'Proc.signal is 0';
    ok $proc.command ~~ List, 'Proc.command is a List';
}

# .write sends data to stdin
{
    my $p6 = Proc::Async.new(:w, "cat");
    my $out = '';
    $p6.stdout.tap: { $out ~= $^a };
    my $pr = $p6.start;
    await $p6.write("hello from stdin".encode);
    $p6.close-stdin;
    await $pr;
    is $out, "hello from stdin", '.write sends data to stdin';
}

# .Supply merges stdout and stderr streams
{
    my $p7 = Proc::Async.new($*EXECUTABLE, '-e', 'say "boo"; note "boo";');
    my $merged = '';
    $p7.Supply.tap({ $merged ~= $_ });
    await $p7.start;
    is $merged, "boo\nboo\n", '.Supply merges stdout and stderr';
}

# .Supply and .stdout/.stderr are mutually exclusive
throws-like {
    my $p8 = Proc::Async.new($*EXECUTABLE);
    $ = $p8.Supply;
    $ = $p8.stdout;
}, X::Proc::Async::SupplyOrStd, 'cannot call .stdout after .Supply';

throws-like {
    my $p9 = Proc::Async.new($*EXECUTABLE);
    $ = $p9.stdout;
    $ = $p9.Supply;
}, X::Proc::Async::SupplyOrStd, 'cannot call .Supply after .stdout';

# bind stdout/stderr to handles
{
    my $in-file = 'proc-async-bind-in.txt';
    my $out-file = 'proc-async-bind-out.txt';
    my $err-file = 'proc-async-bind-err.txt';
    spurt $in-file, "first line\nsecond line\n";

    my $fh-in = open $in-file, :r;
    my $fh-out = open $out-file, :w;
    my $fh-err = open $err-file, :w;

    my $p10 = Proc::Async.new($*EXECUTABLE, '-e', 'note $*IN.get; say $*IN.get');
    $p10.bind-stdin($fh-in);
    $p10.bind-stdout($fh-out);
    $p10.bind-stderr($fh-err);
    await $p10.start;
    .close for $fh-in, $fh-out, $fh-err;

    is slurp($out-file), "second line\n", 'bind-stdout writes captured stdout to handle';
    is slurp($err-file), "first line\n", 'bind-stderr writes captured stderr to handle';
}

throws-like {
    my $p11 = Proc::Async.new($*EXECUTABLE, '-e', 'say 1', :w);
    $p11.bind-stdin($*IN);
}, X::Proc::Async::BindOrUse, 'cannot bind stdin when :w is enabled';

throws-like {
    my $p12 = Proc::Async.new($*EXECUTABLE, '-e', 'say 1');
    $ = $p12.stdout;
    $p12.bind-stdout($*OUT);
}, X::Proc::Async::BindOrUse, 'cannot bind stdout after selecting stdout stream';

throws-like {
    my $p13 = Proc::Async.new($*EXECUTABLE, '-e', 'say 1');
    $p13.bind-stderr($*OUT);
    $ = $p13.Supply;
}, X::Proc::Async::BindOrUse, 'cannot select merged Supply after binding stderr';

{
    my $p14 = Proc::Async.new($*EXECUTABLE, '-e', 'say 1');
    my $stdout = $p14.stdout(:bin);
    my $stderr = $p14.stderr(:bin);
    my $exit = $p14.start;
    ok await($stdout.native-descriptor) > 0, 'stdout(:bin) exposes a native descriptor promise';
    ok await($stderr.native-descriptor) > 0, 'stderr(:bin) exposes a native descriptor promise';
    await $exit;
}

{
    my $p15 = Proc::Async.new($*EXECUTABLE, '-e', 'say "plumbed together"');
    my $p16 = Proc::Async.new($*EXECUTABLE, '-e', '$*IN.get.uc.say');
    $p16.bind-stdin($p15.stdout);
    react {
        my $out = '';
        whenever $p16.stdout {
            $out ~= $_;
        }
        whenever Promise.allof($p15.start, $p16.start) {
            is $out.trim, 'PLUMBED TOGETHER', 'stdout taps stay live when registered before Proc::Async.start';
        }
    }
}

{
    my $p17 = Proc::Async.new($*EXECUTABLE, '-e', 'note "plumbed together err"');
    my $p18 = Proc::Async.new($*EXECUTABLE, '-e', '$*IN.get.uc.say');
    $p18.bind-stdin($p17.stderr);
    react {
        my $out = '';
        whenever $p18.stdout {
            $out ~= $_;
        }
        whenever Promise.allof($p17.start, $p18.start) {
            is $out.trim, 'PLUMBED TOGETHER ERR', 'stderr supplies can feed chained Proc::Async stdin';
        }
    }
}
