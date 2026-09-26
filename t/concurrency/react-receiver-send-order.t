use Test;

plan 2;

# Issue #9611: a react polls its channel-receiver sources (a `whenever
# <Promise>`, a `Proc::Async` output stream) once per round in index order.
# `.start`'s Promise is kept only after both reader threads finished, so the
# final stdout chunk is always queued before the Promise's event -- but if the
# stdout receiver was polled while still empty and both arrived before the
# Promise was polled in the same round, `done` ran first and the chunk was
# lost. Events now carry a global send sequence and are delivered in that
# order. The loss was a race, so each case repeats enough to have hit it.

sub run-once(*@args) {
    my $p = Proc::Async.new: $*EXECUTABLE, '-e', 'print @*ARGS.join("|")', |@args;
    my $out = '';
    react {
        whenever $p.stdout { $out ~= $_ }
        whenever $p.start { done }
    }
    $out
}

my $lost = 0;
for ^25 {
    $lost++ unless run-once('-Ia', '-Ib', 'x') eq '-Ia|-Ib|x';
}
is $lost, 0, 'the final stdout chunk is delivered before `whenever $p.start { done }`';

# The same with the Promise subscription listed first.
sub run-promise-first() {
    my $p = Proc::Async.new: $*EXECUTABLE, '-e', 'print "a"; $*OUT.flush; sleep 0.01; print "b"';
    my $out = '';
    my $stdout = $p.stdout;
    my $started;
    react {
        whenever $stdout { $out ~= $_ }
        $started = $p.start;
        whenever $started { done }
    }
    $out
}

$lost = 0;
for ^10 {
    $lost++ unless run-promise-first() eq 'ab';
}
is $lost, 0, 'output sent before the Promise was kept always reaches its whenever';
