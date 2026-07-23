unit module OpenSSL::Cipher;

use NativeCall;

class SSL_CIPHER is repr('CStruct') {
    has int32 $.valid;
    has Str   $.name;
    has long  $.id;

    has long  $.algorithm_mkey;
    has long  $.algorithm_auth;
    has long  $.algorithm_enc;
    has long  $.algorithm_mac;
    has long  $.algorithm_ssl;

    has long  $.algo_strength;
    has long  $.algorithm2;
    has int32 $.strength_bits;
    has int32 $.alg_bits;
}

# vim: expandtab shiftwidth=4
