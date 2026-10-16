use Test;

# `Proc::Async`'s methods are rows of `Proc::Async`, found by their owner, and
# commit through the instance's shared cell (ADR-11276 §9.128): whatever the
# receiver is (a variable, an array element), every alias sees the state.

plan 9;

{
    my $p = Proc::Async.new('echo', 'one');
    my @procs = $p;
    my $out = '';
    @procs[0].stdout.tap({ $out ~= $_ });
    nok $p.started, 'not started yet';
    my $promise = @procs[0].start;
    ok $p.started, 'start through an array element is seen through the variable';
    await $promise;
    is $out.trim, 'one', 'stdout tapped through the element';
    throws-like { $p.start }, X::Proc::Async::AlreadyStarted, 'a second start';
}

is Proc::Async.new('echo', 'a', 'b').command, <echo a b>, 'command of a temporary';
nok Proc::Async.new('echo').w, 'w without :w';

{
    my $p = Proc::Async.new('echo', 'x');
    $p.Supply;
    throws-like { $p.stdout }, X::Proc::Async::SupplyOrStd, 'Supply then stdout';
}

{
    my $p = Proc::Async.new(:w, 'cat');
    my $out = '';
    $p.stdout.tap({ $out ~= $_ });
    my $done = $p.start;
    await $p.say('hello');
    $p.close-stdin;
    await $done;
    is $out, "hello\n", 'say and close-stdin';
}

ok Proc::Async.new('echo').can('kill'), '.can finds a row';
