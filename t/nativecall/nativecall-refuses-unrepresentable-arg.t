use Test;
use NativeCall;

plan 8;

# A defined argument the native marshaller cannot represent as the declared
# CArray / Blob parameter used to become an empty per-call buffer, whose
# dangling non-NULL pointer reached C -- `frexp(8e0, "abc")` crashed with
# SIGSEGV (#11529). It is refused with a catchable exception now, as rakudo
# refuses it ("expected argument 2 with CArray representation"). An
# undefined argument and an empty array pass NULL, as in MoarVM.

sub frexp(num64, CArray[int32] --> num64) is native { * }
sub memchr(Blob, int32, size_t --> Pointer) is native { * }
sub memchr-carray(CArray[uint8], int32, size_t --> Pointer) is native is symbol('memchr') { * }

my $str = 'abc';
throws-like { frexp(8e0, $str) }, Exception,
    message => 'Native call expected argument 2 with CArray representation, but got a P6opaque (Str)',
    'a Str for a CArray parameter is refused';
my %hash;
throws-like { frexp(8e0, %hash) }, Exception,
    message => 'Native call expected argument 2 with CArray representation, but got a P6opaque (Hash)',
    'a Hash for a CArray parameter is refused';
throws-like { memchr($str, 98, 3) }, Exception,
    message => 'Native call expected argument 1 with VMArray representation, but got a P6opaque (Str)',
    'a Str for a Blob parameter is refused';

nok memchr(Blob, 98, 0).defined, 'a Blob type object passes NULL';
nok memchr(Buf.new, 98, 0).defined, 'an empty Buf passes NULL';
ok memchr('abc'.encode, 98, 3).defined, 'a filled Blob still passes its storage';
nok memchr-carray(CArray[uint8].new, 98, 0).defined, 'an empty CArray passes NULL';
ok memchr-carray(CArray[uint8].new(1, 98, 3), 98, 3).defined,
    'a filled CArray still passes its storage';
