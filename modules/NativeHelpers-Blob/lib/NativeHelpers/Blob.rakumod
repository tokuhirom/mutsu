unit module NativeHelpers::Blob:ver<0.1.13>;

use NativeCall;
use MoarVM::Guts::REPRs;

constant stdlib = Rakudo::Internals.IS-WIN ?? 'msvcrt' !! Str;

our $debug = False;

my sub memcpy(  # UNCOVERABLE
  Pointer $dest, Pointer $src, size_t $size
--> Pointer) is native(stdlib) {*}  # UNCOVERABLE

multi sub pointer-to(Blob:D \blob, :$typed) is export {
    my \t  = blob.^array_type;
    my $bb = BODY_OF(blob);

    note "From ", $bb.raku if $debug;
    my \ptr = $bb.realstart;
    $typed ?? nativecast(Pointer[t], ptr) !! ptr
}

multi sub sizeof(Blob:D \blob) { blob.bytes }

multi sub pointer-to(array:D \arr, :$typed) is export {
    my \t  = arr.^array_type;
    my $bb = BODY_OF(arr);

    note "From ", $bb.raku if $debug;
    my \ptr = $bb.realstart;
    $typed ?? nativecast(Pointer[t], ptr) !! ptr
}

multi sub pointer-to(CArray:D \arr, :$typed) is export {
    my \t  = arr.^array_type;
    my $bb = BODY_OF(arr);

    note "From ", $bb.raku if $debug;
    my \ptr = $bb.storage;
    $typed ?? nativecast(Pointer[t], ptr) !! ptr
}

multi sub sizeof(Mu:D \arr) is export {
    arr.elems * nativesizeof(arr.^array_type)
}

sub ptr-sized(Mu:D \arr) is export {
    \(pointer-to(arr), sizeof(arr))
}

multi sub buf-sized(Blob:D \b) is export {
    \(b, b.bytes)
}

multi sub buf-sized(Str:D \s) is export {
    buf-sized(s.encode);
}

# back compatibility only
sub BPointer(Blob:D \blob, :$typed) is export is DEPRECATED("pointer-to") {
    pointer-to(blob, :$typed);
}

sub carray-from-blob(Blob:D \blob, :$managed) is export {
    my \t  = blob.^array_type;
    my $bb = BODY_OF(blob);

    note "From ", $bb.raku if $debug;
    if $managed {
        my \arr = CArray[t].new;
        arr[$bb.elems - 1] = 0; # Force allocation
        my $cb = BODY_OF(arr);

        note "To ", $cb.raku if $debug;
        memcpy($cb.storage, $bb.realstart, $bb.elems * nativesizeof(t));
        arr
    } 
    else {
        nativecast(CArray[t], $bb.realstart)
    }
}

sub carray-is-managed(CArray:D \arr) is export {
    so BODY_OF(arr).managed;
}

sub blob-allocate(Blob:U \blob, $elems) is export {
    blob.allocate($elems.Int)
}

sub blob-from-pointer(
  Pointer:D \ptr, Int :$elems!, Blob:U :$type = Buf
) is export {
    my sub memcpy(  # UNCOVERABLE
      Blob:D $dest, Pointer $src, size_t $size --> Pointer
    ) is native(stdlib) {*}  # UNCOVERABLE

    my \t = ptr.of ~~ void ?? $type.of !! ptr.of;
    fail "Pointer type don't match Blob type"
      if nativesizeof(t) != nativesizeof($type.of);

    my $b = $type;
    with ptr {
        if $b.can('allocate') {
            $b .= allocate($elems);
        }
        else {
            $b = blob-allocate($b, $elems);
        }
        memcpy($b, ptr, $elems * nativesizeof(t));
    }
    $b
}

sub utf8-from-pointer(Pointer:D \ptr, Int $size) is export {
    blob-from-pointer(ptr, :elems($size), :type(utf8));
}

sub blob-from-carray(CArray:D \arr, Int :$size) is export {
    my \t = arr.^array_type;
    my $cb = BODY_OF(arr);

    die "Need :size for unmanaged CArray" unless $cb.managed || $size;
    my $elems = $cb.elems || +$size;
    blob-from-pointer($cb.storage, :$elems, :type(Buf[t]))
}

# vim: expandtab shiftwidth=4
