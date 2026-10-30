use Test;

plan 6;

# `$proc.stdout.lines.tap(...)` splits the process's output into lines for that
# tap alone, whether the tap is registered before `.start` (streamed as the
# child writes) or the process has already run (replayed). A raw tap on the
# same stream still gets whole chunks.

{
    my $p = Proc::Async.new(:w, 'cat');
    my @lines;
    my $done = Promise.new;
    $p.stdout.lines.tap({ @lines.push($_) }, done => { $done.keep(True) });
    my $run = $p.start;
    await $p.write("alpha\nbeta\n".encode);
    await $p.write("gam".encode);
    await $p.write("ma\n".encode);
    $p.close-stdin;
    await $run;
    await Promise.anyof($done, Promise.in(10));
    is-deeply @lines, ['alpha', 'beta', 'gamma'], 'a lines tap taken before start is fed line by line';
    ok $done.status ~~ Kept, 'and its done handler fires when the stream ends';
}

{
    my $p = Proc::Async.new(:w, 'cat');
    my (@lines, @chunks);
    $p.stdout.lines.tap({ @lines.push($_) });
    $p.stdout.tap({ @chunks.push($_) });
    my $run = $p.start;
    await $p.write("one\ntwo\n".encode);
    $p.close-stdin;
    await $run;
    is-deeply @lines, ['one', 'two'], 'a lines tap beside a raw tap still sees lines';
    is @chunks.join, "one\ntwo\n", 'and the raw tap sees the whole stream';
}

{
    my $p = Proc::Async.new('printf', 'a\nb\n');
    my @lines;
    $p.stdout.lines.tap({ @lines.push($_) });
    await $p.start;
    is-deeply @lines, ['a', 'b'], 'lines of a process that ran to completion';
}

{
    my $p = Proc::Async.new('printf', 'x\ny\n');
    my @lines;
    $p.stdout.lines(:!chomp).tap({ @lines.push($_) });
    await $p.start;
    is-deeply @lines, ["x\n", "y\n"], ':!chomp keeps the line terminators';
}
