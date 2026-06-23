use NativeCall;
unit module NCPointerMod;

# A NativeCall binding distributed as a *module* — it references the builtin
# `Pointer` type, which must be available even though the program that `use`s
# this module never mentions `Pointer` itself. Uses libc only (CI-safe).

sub posix_memalign(Pointer $p is rw, int64 $align, int64 $size)
    returns int32 is native('c') { * }
sub free(Pointer $p) is native('c') { * }

#| Allocate `$size` bytes aligned to `$align`; return the (non-NULL) Pointer.
sub alloc-aligned(Int $align, Int $size) is export {
    my $p = Pointer.new;
    my $rc = posix_memalign($p, $align, $size);
    die "posix_memalign failed: $rc" if $rc != 0;
    $p;
}

sub release(Pointer $p) is export { free($p) }
