use v6;

unit module NativeHelpers::Blob:ver<0.1.9>;
use NativeCall;
use MoarVM::Guts::REPRs;
use nqp; # Needed by blob-allocate

constant stdlib = Rakudo::Internals.IS-WIN ?? 'msvcrt' !! Str;

our $debug = False;

my sub memcpy(Pointer $dest, Pointer $src, size_t $size)
    returns Pointer is native(stdlib) { * };

multi sub pointer-to(Blob:D \blob, :$typed) is export {
    my \t = blob.^array_type;
    my $bb = BODY_OF(blob);
    note "From ", $bb.perl if $debug;
    my \ptr = $bb.realstart;
    $typed ?? nativecast(Pointer[t], ptr) !! ptr;
}

multi sub sizeof(Blob:D \blob) {
    blob.bytes;
}

multi sub pointer-to(array:D \arr, :$typed) is export {
    my \t = arr.^array_type;
    my $bb = BODY_OF(arr);
    note "From ", $bb.perl if $debug;
    my \ptr = $bb.realstart;
    $typed ?? nativecast(Pointer[t], ptr) !! ptr;
}

multi sub pointer-to(CArray:D \arr, :$typed) is export {
    my \t = arr.^array_type;
    my $bb = BODY_OF(arr);
    note "From ", $bb.perl if $debug;
    my \ptr = $bb.storage;
    $typed ?? nativecast(Pointer[t], ptr) !! ptr;
}

multi sub sizeof(Mu:D \arr) is export {
    my \t = arr.^array_type;
    arr.elems * nativesizeof(t);
}

sub ptr-sized(Mu:D \arr) is export {
    my $size = sizeof(arr);
    \(pointer-to(arr), $size);
}

multi sub buf-sized(Blob:D \b) is export {
    my $size = b.bytes;
    \(b, $size);
}

multi sub buf-sized(Str:D \s) is export {
    buf-sized(s.encode);
}

# back compatibility only
sub BPointer(Blob:D \blob, :$typed) is export {
    pointer-to(blob, :$typed);
}

our sub carray-from-blob(Blob:D \blob, :$managed) is export {
    my \t = blob.^array_type;
    my $bb = BODY_OF(blob);
    note "From ", $bb.perl if $debug;
    if $managed {
	my \arr = CArray[t].new;
	arr[$bb.elems - 1] = 0; # Force allocation
	my $cb = BODY_OF(arr);
	note "To ", $cb.perl if $debug;
	memcpy($cb.storage, $bb.realstart, $bb.elems * nativesizeof(t));
	arr;
    } else {
	nativecast(CArray[t], $bb.realstart);
    }
}

our sub carray-is-managed(CArray:D \arr) is export {
    so BODY_OF(arr).managed;
}

our sub blob-allocate(Blob:U \blob, $elems) is export {
    my \b = blob.new;
    nqp::setelems(b, nqp::unbox_i($elems.Int));
    b;
}

our sub blob-from-pointer(Pointer:D \ptr, Int :$elems!, Blob:U :$type = Buf) is export {
    my sub memcpy(Blob:D $dest, Pointer $src, size_t $size)
	returns Pointer is native(stdlib) { * };
    my \t = ptr.of ~~ void ?? $type.of !! ptr.of;
    if  nativesizeof(t) != nativesizeof($type.of) {
	fail "Pointer type don't match Blob type";
    }
    my $b = $type;
    with ptr {
	if $b.can('allocate') {
	    $b .= allocate($elems);
	} else {
	    $b = blob-allocate($b, $elems);
	}
	memcpy($b, ptr, $elems * nativesizeof(t));
    }
    $b;
}

our sub utf8-from-pointer(Pointer:D \ptr, Int $size) is export {
    blob-from-pointer(ptr, :elems($size), :type(utf8));
}

our sub blob-from-carray(CArray:D \arr, Int :$size) is export {
    my \t = arr.^array_type;
    my $cb = BODY_OF(arr);
    die "Need :size for unmanaged CArray" unless $cb.managed || $size;
    my $elems = $cb.elems || +$size;
    blob-from-pointer($cb.storage, :$elems, :type(Buf[t]));
}

# vim: ft=perl6:st=4:sw=4
