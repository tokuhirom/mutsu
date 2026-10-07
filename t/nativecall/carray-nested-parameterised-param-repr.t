use Test;
use NativeCall;

# A parameter typed with a nested CArray parameterisation (OpenSSL's
# `d2i_PKCS12(..., CArray[CArray[uint8]] $in, ...)`) must see the same type
# object as the literal: `.REPR` is `CArray`, so upstream's `validnctype` (run by
# `is native`) accepts it. The signature's spelling read `P6opaque`, and every
# load of OpenSSL / Cro::TLS printed "Not an accepted NativeCall type" warnings
# that Rakudo does not. Every expectation was verified against Rakudo.

plan 6;

sub nested(CArray[CArray[uint8]] $x) { }
sub nested-ptr(CArray[Pointer[uint8]] $x) { }
sub single(CArray[uint8] $x) { }

is &nested.signature.params[0].type.REPR, 'CArray', 'a nested CArray parameter has the CArray REPR';
is &nested-ptr.signature.params[0].type.REPR, 'CArray', 'so does a CArray of Pointer[T]';
is &single.signature.params[0].type.REPR, 'CArray', 'and a one-level CArray, as before';
is CArray[CArray[uint8]].REPR, 'CArray', 'the literal agrees';

my @warnings;
{
    CONTROL { when CX::Warn { @warnings.push(.message); .resume } }
    EVAL q:to/RAKU/;
        use NativeCall;
        sub free-nested(CArray[CArray[uint8]] $x) is native('c') is symbol('free') { * }
        sub free-single(CArray[uint8] $x) is native('c') is symbol('free') { * }
        sub free-ptrs(CArray[Pointer] $x) is native('c') is symbol('free') { * }
        RAKU
}
is @warnings.elems, 0, 'declaring is native routines over nested CArray parameters warns about nothing';

lives-ok { EVAL q:to/RAKU/ }, 'a native routine over a nested CArray still declares';
    use NativeCall;
    sub free-again(CArray[CArray[uint8]] $x) is native('c') is symbol('free') { * }
    RAKU
