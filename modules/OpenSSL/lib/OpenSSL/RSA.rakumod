unit module OpenSSL::RSA;

use OpenSSL::NativeLib;
use NativeCall;

our sub RSA_sign(
  int32, Blob, int32, Blob, CArray, OpaquePointer
--> int32) is native(&gen-lib) { ... }

our sub RSA_verify(
  int32, Blob, int32, Blob, int32, OpaquePointer
--> int32) is native(&gen-lib) { ... }

our sub RSA_size(
  OpaquePointer
--> int32) is native(&gen-lib) { ... }

our sub RSA_free(
  OpaquePointer
) is native(&gen-lib) { ... }

# vim: expandtab shiftwidth=4
