unit module OpenSSL::Session;

use NativeCall;

class SSL_SESSION is repr('CStruct') {
    has int32 $.ssl_version;
}

# vim: expandtab shiftwidth=4
