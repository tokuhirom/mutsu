unit module OpenSSL::PEM;

use OpenSSL::NativeLib;
use NativeCall;

our sub PEM_read_bio_RSAPrivateKey(
  OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer
--> OpaquePointer) is native(&gen-lib) { ... }

our sub PEM_read_bio_RSAPublicKey(
  OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer
--> OpaquePointer) is native(&gen-lib) { ... }

our sub PEM_read_bio_RSA_PUBKEY(
  OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer
--> OpaquePointer) is native(&gen-lib) { ... }

our sub PEM_read_bio_X509(
  OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer
--> OpaquePointer) is native(&gen-lib) { ... }

# vim: expandtab shiftwidth=4
