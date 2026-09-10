use Test;

plan 4;

my $path = "tmp-io-out-buffer-{$*PID}.txt".IO;
LEAVE $path.unlink if $path.e;

$path.open(:w).close;
my $fh = $path.open(:w, :out-buffer(10));
$fh.print: 'x' x 15;
$fh.print: 'x' x 4;
is $path.slurp(:bin).elems, 15, 'out-buffer keeps under-limit chunk pending';
$fh.flush;
is $path.slurp(:bin).elems, 19, 'flush writes pending out-buffer content';
$fh.close;

$path.open(:w).close;
$fh = $path.open(:w, :out-buffer(10));
$fh.print: 'x' x 8;
$fh.out-buffer = 5;
is $path.slurp(:bin).elems, 8, 'resizing out-buffer flushes pending bytes';
$fh.close;

$path.open(:w).close;
$fh = $path.open(:rw, :out-buffer(10));
$fh.print: 'x' x 15;
$fh.close;
is $path.slurp(:bin).elems, 15, ':rw handle honors writes with out-buffer';
