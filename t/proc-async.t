use Test;
plan 4;

my $p = Proc::Async.new("echo", "hi");
is elems($p.command), 2, 'command stored';
ok !$p.started, 'not started';
my $promise = $p.start;
ok $p.started, 'started';
is $promise.result, 0, 'start returns exit code';
