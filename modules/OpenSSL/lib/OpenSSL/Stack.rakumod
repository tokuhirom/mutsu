unit class OpenSSL::Stack is repr('CStruct');

use NativeCall;
use OpenSSL::NativeLib;
use OpenSSL::Version;

has int32 $.num;
has CArray[CArray[uint8]] $.data;
has int32 $.sorted;
has int32 $.num_alloc;
has Pointer $.comp;

my sub real_symbol(Str $sym) returns Str {
    state Int $v = OpenSSL::Version::version_num();
    state Bool $is_libressl = OpenSSL::Version::version().contains('LibreSSL');
    $v >= 0x10100000 && !$is_libressl ?? "OPENSSL_$sym" !! $sym;
}

our sub sk_num(
  OpenSSL::Stack
--> int32) is native(&gen-lib) is symbol(real_symbol('sk_num')) { ... }

our sub sk_value(
  OpenSSL::Stack, int32
--> Pointer) is native(&gen-lib) is symbol(real_symbol('sk_value')) { ... }

our sub sk_free(
  OpenSSL::Stack
) is native(&gen-lib) is symbol(real_symbol('sk_free')) { ... }

# vim: expandtab shiftwidth=4
