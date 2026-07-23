unit module OpenSSL::X509;

use OpenSSL::NativeLib;
use OpenSSL::Stack;

use NativeCall;

class BUF_MEM is repr('CStruct') {
    has size_t $.length;
    has CArray[uint8] $.data;
    has size_t $.max;
}

class ASN1_OBJECT is repr('CStruct') {
    has CArray[uint8] $.sn;
    has CArray[uint8] $.ln;
    has int32 $.nid;
    has int32 $.length;
    has CArray[uint8] $.data;
    has int32 $.flags;
}

class ASN1_STRING is repr('CStruct') {
    has int32 $.length;
    has int32 $.type;
    has CArray[uint8] $.data;
    has int32 $.flags;
}

class X509_NAME_ENTRY is repr('CStruct') {
    has ASN1_OBJECT $.object;
    has ASN1_STRING $.value;
    has int32 $.set;
    has int32 $.size;
}

class X509_NAME is repr('CStruct') {
    has OpenSSL::Stack $.entries;
    has int32 $.modified;
    has BUF_MEM $.bytes;
    has ulong $.hash;
    has CArray[uint8] $.canon_enc;
    has int32 $.canon_enclen;
}

our sub dump_x509_stack(OpenSSL::Stack $stack, :$FH = $*ERR) {
    for ^$stack.num -> $num {
        my $entries = nativecast(X509_NAME, $stack.data[$num]).entries;

        for ^$entries.num -> $entry {
            my $asn1_value = nativecast(X509_NAME_ENTRY, $entries.data[$entry]).value;

            my $asn1_buf = Buf.new;
            $asn1_buf[$_] = $asn1_value.data[$_] for ^$asn1_value.length;

            say $FH: "ASN1_STRING[$num].$entry: " ~ $asn1_buf.decode;
        }

        say $FH: "";
    }
}

our sub X509_get_pubkey(
  OpaquePointer
--> OpaquePointer) is native(&crypto-lib) { ... }

our sub X509_free(
  OpaquePointer
) is native(&crypto-lib) { ... }

# vim: expandtab shiftwidth=4
