unit module OpenSSL::EVP;

use OpenSSL::NativeLib;
use NativeCall;

our sub EVP_PKEY_get1_RSA(
  OpaquePointer
--> OpaquePointer) is native(&gen-lib) { ... }

our sub EVP_PKEY_free(
  OpaquePointer
) is native(&gen-lib) { ... }

our sub EVP_CIPHER_CTX_new(--> OpaquePointer) is native(&gen-lib) { ... }

our sub EVP_CIPHER_CTX_free(
  OpaquePointer
) is native(&gen-lib) { ... }

our sub EVP_EncryptInit(
  OpaquePointer, OpaquePointer, Blob, Blob
--> int32) is native(&gen-lib) { ... }

our sub EVP_EncryptUpdate(
  OpaquePointer, Blob, CArray[int32], Blob, int32
--> int32) is native(&gen-lib) { ... }

our sub EVP_EncryptFinal(
  OpaquePointer, Blob, CArray[int32]
--> int32) is native(&gen-lib) { ... }

our sub EVP_DecryptInit(
  OpaquePointer, OpaquePointer, Blob, Blob
--> int32) is native(&gen-lib) { ... }

our sub EVP_DecryptUpdate(
  OpaquePointer, Blob, CArray[int32], Blob, int32
--> int32) is native(&gen-lib) { ... }

our sub EVP_DecryptFinal(
  OpaquePointer, Blob, CArray[int32]
--> int32) is native(&gen-lib) { ... }

class evp_cipher_st is repr('CStruct') {
    has int32 $.nid;
    has int32 $.block_size;
    # Default value for variable length ciphers
    has int32 $.key_len;
    has int32 $.iv_len;
    # Various flags
    has ulong $.flags;
    # + various other fields

    method is-variable-length returns Bool {
        constant EVP_CIPH_VARIABLE_LENGTH = 0x8;
        ? ($!flags +& EVP_CIPH_VARIABLE_LENGTH);
    }
}

# ciphers
our sub EVP_aes_128_cbc( --> OpaquePointer) is native(&gen-lib) { ... }
our sub EVP_aes_192_cbc( --> OpaquePointer) is native(&gen-lib) { ... }
our sub EVP_aes_256_cbc( --> OpaquePointer) is native(&gen-lib) { ... }
our sub EVP_aes_128_ctr( --> OpaquePointer) is native(&gen-lib) { ... }

# vim: expandtab shiftwidth=4
