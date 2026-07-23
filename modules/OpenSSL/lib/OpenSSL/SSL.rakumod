unit module OpenSSL::SSL;

use OpenSSL::NativeLib;
use OpenSSL::Bio;
use OpenSSL::Method;
use OpenSSL::Ctx;
use OpenSSL::Stack;

use NativeCall;

class SSL is repr('CStruct') {
    has int32 $.version;
    has int32 $.type;

    has OpenSSL::Method::SSL_METHOD $.method;

    has OpenSSL::Bio::BIO $.rbio;
    has OpenSSL::Bio::BIO $.wbio;
    has OpenSSL::Bio::BIO $.bbio;

    has int32 $.rwstate;

    has int32 $.in_handshake;

    # function
    has OpaquePointer $.handshake_func;

    has int32 $.server;

    has int32 $.new_session;

    has int32 $.quiet_shutdown;
    has int32 $.shutdown;

    has int32 $.state;
    has int32 $.rstate;
}

our sub SSL_library_init() is native(&ssl-lib)  { ... }

our sub OPENSSL_init_ssl(
  uint64, OpaquePointer
) is native(&ssl-lib) { ... }

our sub SSL_load_error_strings() is native(&ssl-lib) { ... }

our sub SSL_new(
  OpenSSL::Ctx::SSL_CTX
--> SSL) is native(&ssl-lib) { ... }

our sub SSL_set_fd(
  SSL, int32
--> int32) is native(&ssl-lib) { ... }

our sub SSL_shutdown(
  SSL
--> int32) is native(&ssl-lib) { ... }

our sub SSL_free(SSL) is native(&ssl-lib) { ... }

our sub SSL_get_error(
  SSL, int32
--> int32) is native(&ssl-lib) { ... }

our sub SSL_accept(
  SSL
--> int32) is native(&ssl-lib) { ... }

our sub SSL_connect(
  SSL
--> int32) is native(&ssl-lib) { ... }

our sub SSL_read(
  SSL, Blob, int32
--> int32) is native(&ssl-lib)  { ... }

our sub SSL_write(
  SSL, Blob, int32
--> int32) is native(&ssl-lib) { ... }

our sub SSL_set_connect_state(
  SSL
) is native(&ssl-lib) { ... }

our sub SSL_set_accept_state(
  SSL
) is native(&ssl-lib) { ... }

our sub SSL_set_bio(
  SSL, OpaquePointer, OpaquePointer
--> int32) is native(&ssl-lib) { ... }

our sub SSL_load_client_CA_file(
  CArray[uint8]
--> OpenSSL::Stack) is native(&ssl-lib)  { ... }

our sub SSL_get_client_CA_list(
  SSL
--> OpenSSL::Stack) is native(&ssl-lib) { ... }

our sub SSL_set_client_CA_list(
  SSL, OpenSSL::Stack
) is native(&ssl-lib) { ... }

# long SSL_ctrl(SSL *ssl, int cmd, long larg, void *parg)
our sub SSL_ctrl(
  SSL, int32, long, Str
--> long) is native(&ssl-lib) { ... }

# vim: expandtab shiftwidth=4
