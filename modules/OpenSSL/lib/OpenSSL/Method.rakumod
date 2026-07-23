unit module OpenSSL::Method;

use OpenSSL::NativeLib;
use NativeCall;

class SSL_METHOD is repr('CStruct') {
    # This field is not present in LibreSSL implementation,
    # more so this structure is not really meant for introspection.
    # But for a CStruct we must provide at least one attribute,
    # so this one is it.
    # When used with OpenSSL implementation, it returns some values,
    # otherwise it returns garbage.
    has int32 $.version;
}

our sub SSLv2_client_method(  --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub SSLv2_server_method(  --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub SSLv2_method(         --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub SSLv3_client_method(  --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub SSLv3_server_method(  --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub SSLv3_method(         --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub SSLv23_client_method( --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub SSLv23_server_method( --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub SSLv23_method(        --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLS_client_method(    --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLS_server_method(    --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLS_method(           --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLSv1_client_method(  --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLSv1_server_method(  --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLSv1_method(         --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLSv1_1_client_method(--> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLSv1_1_server_method(--> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLSv1_1_method(       --> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLSv1_2_client_method(--> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLSv1_2_server_method(--> SSL_METHOD) is native(&ssl-lib) { ... }
our sub TLSv1_2_method(       --> SSL_METHOD) is native(&ssl-lib) { ... }

# vim: expandtab shiftwidth=4
