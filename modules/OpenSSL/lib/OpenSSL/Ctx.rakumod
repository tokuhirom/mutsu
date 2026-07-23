unit module OpenSSL::Ctx;

use OpenSSL::NativeLib;
use OpenSSL::Method;
use NativeCall;

class SSL_CTX is repr('CStruct') {
    has OpenSSL::Method::SSL_METHOD $.method;
}

our sub SSL_CTX_new(
  OpenSSL::Method::SSL_METHOD
--> SSL_CTX) is native(&ssl-lib) { ... }

our sub SSL_CTX_free(
  SSL_CTX
) is native(&ssl-lib) { ... }

our sub SSL_CTX_ctrl(
  SSL_CTX, int32, long, Pointer
--> long) is native(&ssl-lib) { ... }

our sub SSL_CTX_use_certificate(
  SSL_CTX, Pointer
--> int32) is native(&ssl-lib) { ... }

our sub SSL_CTX_use_certificate_file(
  SSL_CTX, Str, int32
--> int32) is native(&ssl-lib) { ... }

our sub SSL_CTX_use_certificate_chain_file(
  SSL_CTX, Str
--> int32) is native(&ssl-lib) { ... }

our sub SSL_CTX_use_PrivateKey(
  SSL_CTX, Pointer
--> int32) is native(&ssl-lib) { ... }

our sub SSL_CTX_use_PrivateKey_file(
  SSL_CTX, Str, int32
--> int32) is native(&ssl-lib) { ... }

our sub SSL_CTX_check_private_key(
  SSL_CTX
--> int32) is native(&ssl-lib) { ... }

# vim: expandtab shiftwidth=4
