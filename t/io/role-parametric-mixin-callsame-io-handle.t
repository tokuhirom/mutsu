use v6;
use Test;

# From LN (IO::CatHandle::AutoLines): a parameterized role mixed into an open
# handle (`$*ARGFILES does R[:reset]`) overrides `get` and reaches the native
# method through `callsame`. The mixin stayed a wrapper over the plain handle,
# and the callsame base returned Nil instead of reading a line.

plan 6;

role Counter[Bool:D :$reset = True, :$LN] {
    has Int:D $.ln is rw = 0;
    method get {
        my \v = callsame;
        v === Nil ?? ($!ln = 0) !! ++$!ln;
        v
    }
}

my $path = $*TMPDIR.add("mutsu-callsame-handle-{$*PID}.txt");
$path.spurt("foo\nbar\n");
LEAVE $path.unlink;

my $fh = $path.open;
$fh does Counter[:reset(True), :LN];
is $fh.get, 'foo', 'parameterized role: callsame reads the first line';
is $fh.ln, 1, 'and the role counted it';
is $fh.get, 'bar', 'the second line';
is $fh.ln, 2, 'counted again';
nok $fh.get.defined, 'end of input is not a line';
is $fh.ln, 0, 'and resets the counter';
$fh.close;
