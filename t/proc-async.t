use Test;
plan 11;

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
    ok $proc.command ~~ Array, 'Proc.command is an Array';
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
