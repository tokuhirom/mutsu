use Test;

# A named sub resolves a captured outer lexical once per call and reads it
# from the cell after that (ADR-18244 stage B). Every read still sees the
# variable's current value: writes made by the sub itself, by a callee, by a
# recursive activation, or by the mainline in between calls.

plan 10;

my $n = 1;
sub bump() { $n++ }
sub reads-around-a-call() { my $a = $n; bump(); my $b = $n; "$a,$b" }
is reads-around-a-call(), '1,2', 'a callee write shows on the next read';
is reads-around-a-call(), '2,3', 'and on a warm call';

my $m = 10;
sub other() { $m + 0 }
sub caller-keeps() { my $a = $n; my $o = other(); $n += 5; my $b = $n; "$a,$o,$b" }
is caller-keeps(), '3,10,8', 'a caller still reads its own lexical after a callee read another';

sub depth($d) {
    my $here = $n;
    $n += 100 if $d > 0;
    my $inner = $d > 0 ?? depth($d - 1) !! '';
    "$here/{$n}" ~ ($inner ?? " $inner" !! '')
}
is depth(2), '8/208 108/208 208/208', 'recursive activations each read the shared variable';

$n = 0;
is reads-around-a-call(), '0,1', 'a mainline write between calls is seen';

{
    my $x = 'block';
    sub in-block() { $x }
    is in-block(), 'block', 'a bare-block sub reads its block lexical';
    $x = 'changed';
    is in-block(), 'changed', 'and sees a later write';
    {
        my $x = 'shadow';
        is in-block(), 'changed', 'a shadowing block does not leak into the sub';
    }
}

my @log;
sub logger($v) { @log.push: $v; $n }
sub loop-reader() { my @seen; for ^3 { @seen.push: logger($_); $n++ }; @seen.join(',') }
is loop-reader(), '1,2,3', 'reads inside a loop with calls in between';
is @log.join(','), '0,1,2', 'the aggregate collected every call';
