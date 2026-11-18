use Test;

# `Proc::Async.write` (and `.print`/`.say`/`.put`) breaks its Promise when the
# bytes cannot reach the child, as Rakudo does: `broken pipe` once the child
# closed its stdin, `Cannot write to process that has already terminated` once
# it was reaped. The Promise was kept regardless, so a feeder that writes chunk
# by chunk never learned the child had stopped reading (Rakuish
# t/48-exec-stdin.t `in-early`).

plan 6;

sub await-message($p) {
    my $msg = '';
    try { await $p; CATCH { default { $msg = .message } } };
    $msg
}

{
    my $proc = Proc::Async.new(:w, '/bin/sh', '-c', 'exit 0');
    await $proc.start;
    my $w = $proc.write("hello\n".encode);
    is await-message($w), 'Cannot write to process that has already terminated',
        'write after the child was reaped';
    is $w.status, Broken, 'its Promise is Broken';
    is await-message($proc.print("x")), 'Cannot write to process that has already terminated',
        'print too';
}

{
    my $proc = Proc::Async.new(:w, '/bin/sh', '-c', 'exec 0<&-; sleep 1');
    my $done = $proc.start;
    sleep 0.3;
    is await-message($proc.write("hello\n".encode)), 'broken pipe',
        'write after the child closed its stdin';
    await $done;
}

{
    my $proc = Proc::Async.new(:w, '/bin/sh', '-c', 'head -n 1');
    my $out = '';
    $proc.stdout.tap(-> $s { $out ~= $s });
    my $done = $proc.start;
    my $n = 0;
    my $ok = True;
    while $ok && $n < 200 {
        $n++;
        $ok = (try { await $proc.write(("row$n\n" x 2000).encode); True }) // False;
    }
    await $done;
    ok $n < 200, 'a chunked feeder stops once the child stopped reading';
    is $out, "row1\n", 'the child read its line';
}
