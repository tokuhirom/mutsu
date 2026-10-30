unit module ExportHookLateNativeUser;
use NativeCall;

# `memcpy` is declared (and its `is native` trait applied) each time `copy-bytes`
# runs, from whichever unit calls it.
sub copy-bytes(Pointer:D \src, Int :$elems!) is export {
    my sub memcpy(Blob:D $dest, Pointer $src, size_t $size --> Pointer) is native(Str) {*}
    my $b = Buf.allocate($elems);
    memcpy($b, src, $elems);
    $b
}

sub pointer-to-bytes(Buf $b) is export { nativecast(Pointer, $b) }
