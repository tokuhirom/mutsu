use v6;
use Test;

# docs/per-task-clone-slimming.md slice 6: a `start` block must inherit the
# spawning scope's dynamic IO vars ($*OUT/$*ERR), like rakudo, instead of
# having init_io_environment clobber the thread clone's env entries with
# fresh default handles. Oracle: raku prints captured=[X] for each shape.

plan 4;

my $out = "";
{
    my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush {} }.new;
    await start { print "X" };
}
is $out, "X", 'start inherits the redirected $*OUT (print)';

my $err = "";
{
    my $*ERR = class { method print(*@a) { $err ~= @a.join }; method flush {} }.new;
    await start { $*ERR.print("E") };
}
is $err, "E", 'start inherits the redirected $*ERR (explicit .print)';

my $out2 = "";
{
    my $*OUT = class { method print(*@a) { $out2 ~= @a.join }; method flush {} }.new;
    await start { say 42 };
}
is $out2, "42\n", 'start inherits the redirected $*OUT (say routes through it)';

my $out3 = "";
{
    my $*OUT = class { method print(*@a) { $out3 ~= @a.join }; method flush {} }.new;
    my $p = Promise.start({ print "P" });
    await $p;
}
is $out3, "P", 'Promise.start inherits the redirected $*OUT';
