use v6.e.PREVIEW;
use NativeCall;
use Test;

# From the Net::Ethereum distribution (Compress::Bzip2's internalBlobToBlob):
# a native `is rw` scalar out-parameter passed from a local of a *sub* must be
# written back to that local. The interpreter-path call baked the caller slot
# under the wrong frame depth, so the write-back was dropped whenever the
# calling sub itself ran in a call frame. zlib's `compress` has the same shape
# (`Blob dest, ulong is rw destLen, Blob src, ulong srcLen`).

plan 5;

sub compress(Blob, ulong is rw, Blob, ulong --> int32) is native('z', v1) {*}

my $input = buf8.new(65 xx 5000);

sub from-sub-untyped($d) {
    my $len = 10000;
    my buf8 $out .= new;
    $out[10000] = 0;
    my $rc = compress($out, $len, $d, $d.elems);
    ($rc, $len)
}
sub from-sub-typed(buf8 $d) {
    my $len = 10000;
    my buf8 $out .= new;
    $out[10000] = 0;
    compress($out, $len, $d, $d.elems);
    $len
}

my $top-len = 10000;
my buf8 $top-out .= new;
$top-out[10000] = 0;
is compress($top-out, $top-len, $input, 5000), 0, 'compress succeeds at file scope';
ok $top-len < 100, 'file-scope length is written back';

my ($rc, $len) = from-sub-untyped($input);
is $rc, 0, 'compress succeeds from a sub';
ok $len < 100, 'local of an untyped-parameter sub receives the length';
ok from-sub-typed($input) < 100, 'local of a typed-parameter sub receives the length';
