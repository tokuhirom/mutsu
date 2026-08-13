=begin pod

=head1 NAME

IO::Socket::Async::SSL - Provides an API like IO::Socket::Async, but with TLS support

=head1 SYNOPSIS

Client:

=begin code :lang<raku>

use IO::Socket::Async::SSL;

my $conn = await IO::Socket::Async::SSL.connect('raku.org', 443);
$conn.print: "GET / HTTP/1.0\r\nHost: raku.org\r\n\r\n";
react {
    whenever $conn {
        .print
    }
}
$conn.close;

=end code

Server (assumes certificate and key files C<server-crt.pem> and
C<server-key.pem>):

=begin code :lang<raku>

use IO::Socket::Async::SSL;

react {
    my %ssl-config =
      certificate-file => 'server-crt.pem',
      private-key-file => 'server-key.pem';

    whenever IO::Socket::Async::SSL.listen(
      'localhost', 4433, |%ssl-config
    ) -> $conn {
        my $req = '';
        whenever $conn {
            $req ~= $_;
            if $req.contains("\r\n\r\n") {
                say $req.lines[0];
                await $conn.print(
                  "HTTP/1.0 200 OK\r\nContent-type: text/html\r\n\r\n"
                  ~ "<strong>Hello from a Raku HTTP server</strong>\n");
                $conn.close;
            }
        }
    }
}

=end code

=head1 DESCRIPTION

This module provides a secure sockets implementation with an API very
much like that of the Raku built-in C<IO::Socket::Async> class. For the
client case, provided the standard certificate and host verification
are sufficient, it is drop-in replacement. The server case only needs
two extra arguments to C<listen>, specifying the server key and
certificate.

As with C<IO::Socket::Async>, it is safe to have concurrent connections
and to share them across threads.

=head2 Client

The C<connect> method on C<IO::Socket::Async::SSL> is used to establish
a SSL connection to a server. It requires two positional arguments,
which specify the C<host> and C<port> to connect to. It returns a
C<Promise>, which will be kept with an C<IO::Socket::Async::SSL>
instance when the connection is established and the SSL handshake
completed.

=begin code :lang<raku>

my $conn = await IO::Socket::Async::SSL.connect($host, $port);

=end code

By default, the SSL certificate will be verified, using the default
set of accepted Certificate Authorities. The C<Promise> returned by
C<connect> will be broken if verification fails.

Sometimes it is convenient to create a Certificate Authority (CA)
and use it to sign certificates for internal use, for example to
secure communications between a set of services on an internal
network. In this case, the C<ca-file> named argument can be passed
to specify the certificate authority certificate file:

=begin code :lang<raku>

my $ca-file = '/config/ca-crt.pem';
my $conn = await IO::Socket::Async::SSL.connect(
  'user-service', 443, :$ca-file
);

=end code

Alternatively, a C<ca-path> argument can be specified, indicating
a directory where one or more certificates may be found.

It is possible to disable certificate verification by passing the
C<insecure> named argument a true value. As the name suggests,
B<this is not a secure configuration>, since there is no way for
the client to be sure that it is communicating with the intended
server. Therefore, it is vulnerable to man-in-the-middle attacks.

For control over the ciphers that may be used, pass the C<ciphers>
argument to `connect`. It should be a string in
L<OpenSSL cipher list format|https://www.openssl.org/docs/man1.0.2/apps/ciphers.html>.

If wishing to view encrypted traffic with a tool such as Wireshark
for debugging purposes, pass a filename to the C<ssl-key-log-file>
name argument.  Session keys will be written to this log file, and
Wireshark can then be configured to introspect the encrypted traffic
(Preferences -> Protocols -> TLS -> (Pre-)-Master-Secret log filename).
Note that this key exposure compromises the security of the session!

=head2 Server

The C<listen> method returns a C<Supply> that, when tapped, will
start an SSL server. The server can be shut down by closing the tap.

Whenever a connection is made to the server, the C<Supply> will emit
an C<IO::Socket::Async::SSL> instance. The C<listen> method requires
two positional arguments, specifying the C<host> and C<port> to
listen on. Two named arguments are also required, providing the
C<certificate-file> and C<private-key-file>.

=begin code :lang<raku>

my %ssl-config =
  certificate-file => 'server-crt.pem',
  private-key-file => 'server-key.pem';
my $connections = IO::Socket::Async::SSL.listen(
  'localhost', 4433, |%ssl-config
);

react {
    my $listener = do whenever $connections -> $conn {
        say "Got a connection!";
        $conn.close;
    }

    whenever signal(SIGINT) {
        say "Shutting down...";
        $listener.close;
        exit;
    }
}

=end code

For control over the ciphers that may be used, pass the C<ciphers>
named argument to C<connect>.  It should be a string in
L<OpenSSL cipher list format|https://www.openssl.org/docs/man1.0.2/apps/ciphers.html>.

The following boolean options are also accepted:

=item C<prefer-server-ciphers> - indicates that the order of the
ciphers list as configured on the server should be preferred over
that of the one presented by the client

=item C<no-compression> - disables compression

=item C<no-session-resumption-on-renegotiation>

=head2 Common client and server functionality

Both the C<connect> and C<listen> methods take the following optional
named arguments:

=item C<enc>, which specifies the encoding to use when the socket is
used in character mode. Defaults to C<utf-8>.

=item C<scheduler>, which specifies the scheduler to use for
processing events from the underlying C<IO::Socket::Async> instance.
The default is C<$*SCHEDULER>.  There is rarely a need to change this.

The C<Supply>, C<print>, C<write>, and C<close> methods have the same
semantics as in L<IO::Socket::Async|https://docs.raku.org/type/IO/Socket/Async>.

=head2 Upgrading connections

Some protocols use
L<opportunistic TLS|https://en.wikipedia.org/wiki/Opportunistic_TLS>,
where the decision to use transport layer security is first negotiated
using a non-encrypted protocol - provided negotiation is successful -
a TLS handshake is then performed.

This functionality is provided by the C<upgrade-server> and
C<upgrade-client> methods. Note that the socket to upgrade must be an
instance of C<IO::Socket::Async>. Further, it is important to B<stop
reading from the socket before initiating the upgrade>, which will
typically entail working with the C<Tap> directly, something not
normally needed in C<react>/C<whenever> blocks.

Here is an example of using C<upgrade-server>:

=begin code :lang<raku>

my $server = IO::Socket::Async.listen('localhost', TEST_PORT);
react whenever $server -> $plain-conn {
    my $plain-tap = do whenever $plain-conn.Supply -> $start {
        if $start eq "Psst, let's talk securely!\n" {
            # Must stop reading...
            $plain-tap.close;
            # ...so the module can take over the socket.
            my $enc-conn-handshake = IO::Socket::Async::SSL.upgrade-server(
              $plain-conn,
              private-key-file => 't/certs-and-keys/server.key',
              certificate-file => 't/certs-and-keys/server-bundle.crt'
            );
            whenever $enc-conn-handshake -> $enc-conn {
                uc-service($enc-conn);
            }
            $plain-conn.print("OK, let's talk securely!\n");
        }
        else {
            $plain-conn.print("OK, let's talk insecurely\n");
            uc-service($plain-conn);
        }
    }

    sub uc-service($conn) {
        whenever $conn -> $crypt-text {
            whenever $conn.print($crypt-text.uc) {
                $conn.close;
            }
        }
    }
}

=end code

Here's an example using C<upgrade-client>; again, take note
of the careful handling of the C<Tap>:

=begin code :lang<raku>

my $plain-conn = await IO::Socket::Async.connect('localhost', TEST_PORT);
await $plain-conn.print("Psst, let's talk securely!\n");

react {
    my $plain-tap = do whenever $plain-conn -> $msg {
        $plain-tap.close;
        my $enc-conn-handshake = IO::Socket::Async::SSL.upgrade-client(
          $plain-conn,
          host => 'localhost',
          ca-file => 't/certs-and-keys/ca.crt'
        );
        whenever $enc-conn-handshake -> $enc-conn {
            await $enc-conn.print("hello!\n");
            whenever $enc-conn.head -> $got {
                print $got; # HELLO!
                done;
            }
        }
    }
}

=end code

=head2 Method reference

=end pod

use OpenSSL;
use OpenSSL::Bio;
use OpenSSL::Ctx;
use OpenSSL::EVP;
use OpenSSL::X509;
use OpenSSL::SSL;
use OpenSSL::Stack;
use OpenSSL::Err;

# XXX Contribute these back to the OpenSSL binding.
use OpenSSL::NativeLib;
use NativeCall;
sub BIO_new(OpenSSL::Bio::BIO_METHOD) returns OpaquePointer is native(&gen-lib) {*}
sub BIO_s_mem() returns OpenSSL::Bio::BIO_METHOD is native(&gen-lib) {*}
sub SSL_do_handshake(OpenSSL::SSL::SSL) returns int32 is native(&ssl-lib) {*}
sub SSL_CTX_set_default_verify_paths(OpenSSL::Ctx::SSL_CTX) is native(&ssl-lib) {*}
sub SSL_CTX_load_verify_locations(OpenSSL::Ctx::SSL_CTX, Str, Str) returns int32
    is native(&ssl-lib) {*}
sub SSL_get_verify_result(OpenSSL::SSL::SSL) returns int32 is native(&ssl-lib) {*}
sub SSL_CTX_set_cipher_list(OpenSSL::Ctx::SSL_CTX, Str) returns int32
    is native(&ssl-lib) {*}

sub d2i_PKCS12(Pointer, CArray[CArray[uint8]], long) returns Pointer is native(&gen-lib) {*}
sub PKCS12_parse(Pointer, Str, CArray[Pointer], CArray[Pointer], CArray[Pointer])
    returns int32 is native(&gen-lib) {*}

sub SSL_CTX_set_keylog_callback(OpenSSL::Ctx::SSL_CTX, &callback (
                                        OpenSSL::SSL::SSL,    # ssl
                                        Str))                 # line
    is native(&gen-lib) {*}

my constant SSL_TLSEXT_ERR_OK = 0;
my constant SSL_TLSEXT_ERR_ALERT_FATAL = 2;
my constant SSL_TLSEXT_ERR_NOACK = 3;

my constant %VERIFY_FAILURE_REASONS = %(
     2 => 'unable to get issuer certificate',
     3 => 'unable to get certificate CRL',
     4 => 'unable to decrypt certificate\'s signature',
     5 => 'unable to decrypt CRL\'s signature',
     6 => 'unable to decode issuer public key',
     7 => 'certificate signature failure',
     8 => 'CRL signature failure',
     9 => 'certificate is not yet valid',
     10 => 'certificate has expired',
     11 => 'CRL is not yet valid',
     12 => 'CRL has expired',
     13 => 'format error in certificate\'s notBefore field',
     14 => 'format error in certificate\'s notAfter field',
     15 => 'format error in CRL\'s lastUpdate field',
     16 => 'format error in CRL\'s nextUpdate field',
     17 => 'out of memory',
     18 => 'self signed certificate',
     19 => 'self signed certificate in certificate chain',
     20 => 'unable to get local issuer certificate',
     21 => 'unable to verify the first certificate',
     22 => 'certificate chain too long',
     23 => 'certificate revoked',
     24 => 'invalid CA certificate',
     25 => 'path length constraint exceeded',
     26 => 'unsupported certificate purpose',
     27 => 'certificate not trusted',
     28 => 'certificate rejected',
     29 => 'subject issuer mismatch',
     30 => 'authority and subject key identifier mismatch',
     31 => 'authority and issuer serial number mismatch',
     32 => 'usage does not include certificate signing',
     50 => 'application verification failure',
);
my constant SSL_GET_PEER_CERT_SYMBOL = OpenSSL::Version::version_num() < 0x30000000
                                            ?? "SSL_get_peer_certificate"
                                            !! "SSL_get1_peer_certificate";
sub SSL_get_peer_certificate(OpenSSL::SSL::SSL) returns Pointer
    is native(&ssl-lib)
    is symbol(SSL_GET_PEER_CERT_SYMBOL) {*}
sub X509_get_ext_d2i(Pointer, int32, CArray[int32], CArray[int32]) returns OpenSSL::Stack
    is native(&gen-lib) {*}
sub ASN1_STRING_to_UTF8(CArray[CArray[uint8]], Pointer) returns int32
    is native(&gen-lib) {*}

# ALPN
sub SSL_CTX_set_alpn_protos(OpenSSL::Ctx::SSL_CTX, Buf, uint32) returns int32
    is native(&gen-lib) {*}
sub SSL_CTX_set_alpn_select_cb(OpenSSL::Ctx::SSL_CTX, &callback (
                                   OpenSSL::SSL::SSL,        # ssl
                                   CArray[CArray[uint8]],    # out
                                   CArray[uint8],            # outlen
                                   CArray[uint8],            # in
                                   uint8,                    # inlen
                                   Pointer --> int32),       # arg
                               Pointer)
    is native(&gen-lib) {*}
sub SSL_get0_alpn_selected(OpenSSL::SSL::SSL, CArray[CArray[uint8]], uint32 is rw)
    is native(&gen-lib) {*}

my class GENERAL_NAME is repr('CStruct') {
    has int32 $.type;
    has Pointer $.data;
}
my enum GENERAL_NAME_TYPE <
    GEN_OTHERNAME GEN_EMAIL GEN_DNS GEN_X400 GEN_DIRNAME GEN_EDIPARTY
    GEN_URI GEN_IPADD GEN_RID
>;
my constant SSL_CTRL_SET_TMP_DH = 3;
my constant SSL_CTRL_SET_TMP_ECDH = 4;
my constant SSL_CTRL_OPTIONS = 32;
my constant SSL_CTRL_SET_TLSEXT_HOSTNAME = 55;
my constant NID_subject_alt_name = 85;

my constant SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION = 0x10000;
my constant SSL_OP_NO_COMPRESSION = 0x20000;
my constant SSL_OP_CIPHER_SERVER_PREFERENCE = 0x400000;

# DH setup-related bits, so we can use ciphers that need this
my constant BIGNUM = Pointer;
my class DH_1_0 is repr('CStruct') {
    has int32 $.pad;
    has int32 $.version;
    has BIGNUM $.p;
    has BIGNUM $.g;
    has int32 $.length is rw;
    # There are more fields in this struct, but we don't care about them in this
    # module.

    method set-p(\p) { $!p := p }
    method set-g(\g) { $!g := g }
};
my constant DH = Pointer;
sub DH_new() returns DH is native(&gen-lib) {*}
sub DH_set0_pqg(DH, BIGNUM, BIGNUM, BIGNUM) is native(&gen-lib) {*};
sub DH_get0_p(DH) returns BIGNUM is native(&gen-lib) {*};
sub DH_get0_g(DH) returns BIGNUM is native(&gen-lib) {*};
sub DH_set_length(DH, int32) returns int32 is native(&gen-lib) {*};
sub DH_get_length(DH) returns int32 is native(&gen-lib) {*};
sub DH_free(DH) is native(&gen-lib) {*}
sub BN_bin2bn(Blob, int32, BIGNUM) returns BIGNUM is native(&gen-lib) {*}
sub SSL_CTX_ctrl_DH(OpenSSL::Ctx::SSL_CTX, int32, int32, DH) is symbol('SSL_CTX_ctrl')
    returns int32 is native(&ssl-lib) {*}
sub get_dh2048() returns DH {
    # Based on output from `openssl dhparam -dsaparam -C 2048`
    my constant dh2048_p = Blob.new:
            0xD3,0x7F,0x47,0x2C,0xAF,0x38,0xD3,0x98,0x2E,0x89,
            0x92,0xB9,0x06,0x3A,0x67,0x2A,0xC2,0x0F,0x01,0x55,
            0xB0,0xD6,0x16,0xF3,0x52,0x81,0x99,0xB3,0xD6,0xBC,
            0xE7,0x88,0xD2,0x6F,0x2A,0x21,0x63,0x79,0x24,0xCF,
            0xCF,0x43,0xCE,0x02,0xFC,0x67,0x37,0xB4,0x94,0xDE,
            0xE4,0x47,0x0F,0x70,0xF9,0xB1,0x9E,0x5B,0xA8,0xD7,
            0xC3,0x56,0x27,0xBC,0xE2,0xCE,0x94,0x5D,0xD5,0x01,
            0xE0,0xE3,0x47,0x8F,0x37,0xBA,0xCC,0xE2,0x77,0x7C,
            0x3B,0x18,0x15,0x57,0x80,0x7F,0xCC,0x77,0xB5,0x26,
            0x55,0x13,0x5F,0xC7,0xAE,0xDF,0xE0,0x5F,0x82,0xB3,
            0x75,0xDE,0x8D,0x1F,0x5F,0xF6,0x9E,0x80,0x1B,0x06,
            0xA5,0x1B,0x8B,0xE5,0x46,0xFA,0x8C,0x8A,0x89,0xDF,
            0x36,0x24,0x01,0x79,0xD2,0xC3,0x23,0x9C,0xAA,0xE8,
            0x30,0x98,0x67,0xCF,0xC9,0xA3,0x83,0xE1,0x58,0xB4,
            0xED,0x8F,0xD3,0xF8,0x58,0xF1,0xF8,0x83,0x97,0xE1,
            0x25,0x4F,0x1F,0xE6,0x12,0xDE,0x3F,0xF5,0x88,0x4D,
            0x1D,0x7F,0x85,0x9D,0x3A,0xDF,0xC6,0x8A,0xB8,0xD7,
            0xD8,0xA1,0xFB,0xD4,0xF2,0x24,0x96,0xBF,0x24,0x97,
            0x09,0x71,0x95,0xCD,0xE8,0x36,0x86,0x7F,0x34,0x51,
            0xC0,0x6C,0xB8,0x51,0x6A,0x0C,0xE7,0x44,0x54,0x51,
            0xDA,0xAF,0xFB,0x14,0xC7,0xC5,0xD1,0x47,0x43,0xD6,
            0x04,0x5C,0x1F,0x3E,0x2E,0x9F,0x52,0x94,0x87,0xD9,
            0x66,0x91,0xB7,0x8C,0x7E,0x72,0xB7,0x63,0xCC,0x08,
            0x36,0x4A,0xD2,0x94,0x86,0x58,0x3B,0xDE,0xF0,0x42,
            0x12,0xCC,0xED,0x40,0xF5,0xD9,0xE0,0xA8,0xB5,0x63,
            0x40,0xA6,0xE7,0xAE,0xD8,0xDB;
    my constant dh2048_g = Blob.new:
            0xC9,0x1E,0xB2,0xAE,0x8F,0xC9,0x94,0xD9,0x99,0x85,
            0x49,0x90,0xA8,0x2A,0x77,0xB4,0xE9,0x11,0xDA,0x76,
            0x92,0x76,0x17,0x27,0x75,0xC8,0x7D,0x77,0xF4,0xAA,
            0xA9,0x23,0xD0,0xD9,0x7D,0x05,0x15,0x49,0xEC,0x47,
            0xE3,0xA5,0x01,0x38,0xF8,0x06,0xE8,0x57,0x0D,0x94,
            0x8B,0x3F,0x1C,0x22,0x39,0x2D,0xE3,0xFD,0xE4,0xFE,
            0xC7,0x22,0xA9,0xE4,0xB7,0xFF,0x05,0x27,0x12,0xEC,
            0x8A,0x72,0x5A,0x9D,0xBC,0xB0,0xF2,0x19,0x40,0xA0,
            0x07,0x81,0x47,0x92,0x91,0xD6,0xF6,0xC8,0x34,0xAC,
            0x78,0x8E,0xA1,0x51,0x91,0x98,0xD7,0x23,0x22,0xAD,
            0x7A,0x50,0x9B,0x99,0x88,0x5B,0x2D,0x64,0xD2,0x16,
            0xB2,0x72,0xF2,0x6B,0x4F,0xC7,0x18,0xE6,0x01,0x7E,
            0xDE,0xEA,0xEA,0x1B,0x04,0x0D,0x30,0x08,0xAF,0xCF,
            0x2C,0x16,0xC1,0x24,0xC4,0x69,0x4D,0x84,0x22,0xDB,
            0x25,0xD9,0xA2,0x81,0x4B,0x73,0xF0,0x75,0x09,0x22,
            0xEF,0xE3,0x0A,0x2A,0x2F,0xEC,0xFC,0x37,0x25,0x26,
            0x93,0x9B,0x1A,0x63,0x22,0x4D,0xFF,0x4D,0x27,0x92,
            0x99,0xCE,0xFF,0xB6,0xD0,0xA8,0x89,0x53,0xF1,0x92,
            0x25,0xAA,0xBA,0xA7,0xA1,0xD9,0xDA,0xFB,0x64,0x61,
            0xD3,0xBB,0x82,0x8E,0xD5,0xE0,0xE2,0x58,0x45,0xF9,
            0xDB,0xC7,0xD0,0x48,0x39,0xEA,0x11,0xED,0xFA,0x33,
            0x4A,0xCC,0x19,0x22,0xCD,0x89,0xE3,0x66,0x96,0x1F,
            0x05,0x8C,0xA8,0x41,0x4B,0x37,0xFD,0xDE,0x73,0x35,
            0x78,0x0F,0x0C,0x21,0x39,0x54,0xAB,0x3E,0x17,0xD5,
            0x6C,0xF4,0x52,0xD5,0x18,0x8B,0x0F,0xE7,0x10,0x1C,
            0x3C,0x2C,0x60,0x71,0x73,0xFB;
    my DH $dh = DH_new();
    without $dh {
        fail("Could not allocate DH");
    }
    my \p = BN_bin2bn(dh2048_p, dh2048_p.elems, BIGNUM);
    my \g = BN_bin2bn(dh2048_g, dh2048_g.elems, BIGNUM);
    try {
        DH_set0_pqg($dh, p, BIGNUM.new, g);
        DH_set_length($dh, 256);
        if !DH_get0_p($dh) || !DH_get0_g($dh) || DH_get_length($dh) != 256 {
            DH_free($dh);
            fail("Failed to set up DH");
        }
    }
    if $! {
        my $dh1 = nativecast(DH_1_0, $dh);
        $dh1.set-p(p);
        $dh1.set-g(g);
        $dh1.length = 256;
        if !$dh1.p || !$dh1.g {
            DH_free($dh1);
            fail("Failed to set up DH");
        }
    }
    return $dh;
}

# ECDH setup
my constant EC_KEY = Pointer;
my constant EC_GROUP = Pointer;
my constant NID_X9_62_prime256v1 = 415;
sub EC_KEY_new() returns EC_KEY is native(&gen-lib) {*}
sub EC_KEY_set_group(EC_KEY, EC_GROUP) returns int32 is native(&gen-lib) {*}
sub EC_GROUP_new_by_curve_name(int32) returns EC_GROUP is native(&gen-lib) {*}
sub SSL_CTX_ctrl_ECDH(OpenSSL::Ctx::SSL_CTX, int32, int32, EC_KEY) is symbol('SSL_CTX_ctrl')
    returns int32 is native(&ssl-lib) {*}
sub get_ecdh() {
    my $ecdh = EC_KEY_new();
    without $ecdh {
        fail("Failed to allocate ECDH key");
    }
    if EC_KEY_set_group($ecdh, EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1)) != 1 {
        fail("Failed to set ECDH curve group");
    }
    return $ecdh;
}

# Per OpenSSL module, make a simple call to ensure libeay32.dll is loaded before
# ssleay32.dll on Windows.
OpenSSL::EVP::EVP_aes_128_cbc();

# On first load of the module, initialize the library.
try {
    CATCH {
        default { OpenSSL::SSL::OPENSSL_init_ssl(0, OpaquePointer); }
    }
    OpenSSL::SSL::SSL_library_init();
    OpenSSL::SSL::SSL_load_error_strings();
}

# For now, we'll put a lock around all of our interactions with the library.
# There are smarter things possible.
my $lib-lock = Lock.new;

class X::IO::Socket::Async::SSL is Exception {
    has Str $.message;
}
class X::IO::Socket::Async::SSL::Verification is X::IO::Socket::Async::SSL {}

#| An asynchronous socket with Transport Layer Security. Has an API very
#| similar to the builtin IO::Socket::Async.
class IO::Socket::Async::SSL {
    has IO::Socket::Async $!sock;
    has OpenSSL::Ctx::SSL_CTX $!ctx;
    has OpenSSL::SSL::SSL $!ssl;
    has $!read-bio;
    has $!write-bio;
    has $!connected-promise;
    has $!accepted-promise;
    has $!shutdown-promise;
    has $!closed;
    has $.enc;
    has $.insecure;
    has $!host;
    has $!alpn;
    has $.alpn-result;
    has Supplier::Preserving $!bytes-received;
    has @!outstanding-writes;

    method new() {
        die "Cannot create an asynchronous SSL socket directly; please use\n" ~
            "IO::Socket::Async::SSL.connect or IO::Socket::Async::SSL.listen";
    }

    submethod BUILD(:$!sock, :$!enc, OpenSSL::Ctx::SSL_CTX :$!ctx, :$!ssl,
                    :$!read-bio, :$!write-bio,
                    :$!connected-promise, :$!accepted-promise, :$!host, :$!alpn,
                    :$!insecure = False) {
        $!bytes-received .= new;
        $!sock.Supply(:bin).tap:
            -> Blob $data {
                $lib-lock.protect: {
                    if $!ssl {
                        OpenSSL::Bio::BIO_write($!read-bio, $data, $data.bytes);
                        self!handle-buffers();
                    }
                }
            },
            done => {
                $lib-lock.protect: -> {
                    self!handle-buffers();
                    with $!connected-promise {
                        if .status == Planned {
                            .break: X::IO::Socket::Async::SSL.new:
                                message => 'The socket was closed during negotiation';
                        }
                    }
                }
                $!bytes-received.done;
            },
            quit => {
                $!bytes-received.quit($_);
            };
        self!handle-buffers();
    }

    #| Establish a TLS connection. Returns a Promise that will be kept
    #| with an IO::Socket::Async::SSL instance if the connection is
    #| successful, or broken if the connection fails.
    method connect(IO::Socket::Async::SSL:U: Str() $host, Int() $port,
                   :$enc = 'utf8', :$scheduler = $*SCHEDULER,
                   OpenSSL::ProtocolVersion :$version = -1,
                   :$ca-file, :$ca-path, :$insecure, :$alpn,
                   Str :$ciphers, :$ssl-key-log-file --> Promise) {
        self!client-setup:
            { IO::Socket::Async.connect($host, $port, :$scheduler) },
            :$enc, :$version, :$ca-file, :$ca-path, :$insecure,
            :$alpn, :$ciphers, :$host, :$ssl-key-log-file;
     }

    #| Upgrade an existing client socket to TLS. This is useful when
    #| implementing StartTLS. It is important that the plaintext tap
    #| of the asynchronous socket's Supply is closed, so that it can
    #| be re-tapped by this module. Returns a Promise that will be
    #| kept with an IO::Socket::Async::SSL instance provided the
    #| upgrade succeeds, or broken if it fails.
    method upgrade-client(IO::Socket::Async::SSL:U: IO::Socket::Async:D $conn,
                          :$enc = 'utf8', OpenSSL::ProtocolVersion :$version = -1,
                          :$ca-file, :$ca-path, :$insecure, :$alpn,
                          Str :$ciphers, Str :$host, :$ssl-key-log-file --> Promise) {
        self!client-setup:
            { Promise.kept($conn) },
            :$enc, :$version, :$ca-file, :$ca-path, :$insecure,
            :$alpn, :$ciphers, :$host, :$ssl-key-log-file;
     }

     method !client-setup(&connection-source,
                          :$enc = 'utf8', OpenSSL::ProtocolVersion :$version,
                          :$ca-file, :$ca-path, :$insecure, :$alpn, :$ciphers,
                          Str :$host, :$ssl-key-log-file) {
        start {
            my $sock = await connection-source();
            my $connected-promise = Promise.new;
            $lib-lock.protect: {
                my $ctx = self!build-client-ctx($version);
                with $ssl-key-log-file {
                    SSL_CTX_set_keylog_callback($ctx, sub ($ssl, $line) {
                        $ssl-key-log-file.IO.spurt: $line ~ "\n", :append;
                        CATCH {
                            default {
                                note "Failed to write TLS keys to $ssl-key-log-file. Reason:";
                                note .message;
                            }
                        }
                    });
                }

                SSL_CTX_set_default_verify_paths($ctx);
                if defined($ca-file) || defined($ca-path) {
                    SSL_CTX_load_verify_locations($ctx,
                        defined($ca-file) ?? $ca-file.Str !! Str,
                        defined($ca-path) ?? $ca-path.Str !! Str);
                }
                with $ciphers {
                    if SSL_CTX_set_cipher_list($ctx, $ciphers) == 0 {
                        die "No ciphers from the provided list were selected";
                    }
                }
                if $alpn.defined {
                    my $buf = build-protocol-list(@$alpn);
                    SSL_CTX_set_alpn_protos($ctx, $buf, $buf.elems);
                }
                my $ssl = OpenSSL::SSL::SSL_new($ctx);
                my $read-bio = BIO_new(BIO_s_mem());
                my $write-bio = BIO_new(BIO_s_mem());
                check($ssl, OpenSSL::SSL::SSL_set_bio($ssl, $read-bio, $write-bio));
                with $host {
                    OpenSSL::SSL::SSL_ctrl($ssl, SSL_CTRL_SET_TLSEXT_HOSTNAME, 0, $host);
                }
                OpenSSL::SSL::SSL_set_connect_state($ssl);
                check($ssl, SSL_do_handshake($ssl));
                CATCH {
                    OpenSSL::SSL::SSL_free($ssl) if $ssl;
                    OpenSSL::Ctx::SSL_CTX_free($ctx) if $ctx;
                }
                self.bless(
                    :$sock, :$enc, :$ctx, :$ssl, :$read-bio, :$write-bio,
                    :$connected-promise, :$host, :$insecure, :$alpn
                )
            }
            await $connected-promise;
        }
    }

    method !build-client-ctx($version) {
        my $method = do given $version {
            when 2 { OpenSSL::Method::SSLv2_client_method() }
            when 3 { OpenSSL::Method::SSLv3_client_method() }
            when 1 { OpenSSL::Method::TLSv1_client_method() }
            when 1.1 { OpenSSL::Method::TLSv1_1_client_method() }
            when 1.2 { OpenSSL::Method::TLSv1_2_client_method() }
            default {
                try { OpenSSL::Method::TLSv1_2_client_method() } ||
                    try { OpenSSL::Method::TLSv1_client_method() }
            }
        }
        OpenSSL::Ctx::SSL_CTX_new($method)
    }

    #| Open a socket on the specified host and port, and start listening
    #| for incoming TLS connections. Returns a Supply, upon which
    #| successfully established incoming TLS connections will be
    #| emitted.
    method listen(IO::Socket::Async::SSL:U: Str() $host, Int() $port,
                  Int() $backlog = 128,
                  :$enc = 'utf8', :$scheduler = $*SCHEDULER,
                  OpenSSL::ProtocolVersion :$version = -1,
                  :$certificate-file, :$private-key-file, :$alpn,
                  Str :$ciphers, :$prefer-server-ciphers, :$no-compression,
                  :$no-session-resumption-on-renegotiation --> Supply) {
        self!server-setup:
            IO::Socket::Async.listen($host, $port, $backlog, :$scheduler),
            :$enc, :$version, :$certificate-file, :$private-key-file,
            :$alpn, :$ciphers, :$prefer-server-ciphers, :$no-compression,
            :$no-session-resumption-on-renegotiation;
    }

    #| Upgrade an existing server socket to TLS. This is useful when
    #| implementing StartTLS. It is important that the plaintext tap
    #| of the asynchronous socket's Supply is closed, so that it can
    #| be re-tapped by this module. Returns a Supply that will emit
    #| IO::Socket::Async::SSL instance provided the upgrade succeeds,
    #| or quit if it fails.
    method upgrade-server(IO::Socket::Async::SSL:U: IO::Socket::Async:D $socket,
                  :$enc = 'utf8', OpenSSL::ProtocolVersion :$version = -1,
                  :$certificate-file, :$private-key-file, :$alpn,
                  Str :$ciphers, :$prefer-server-ciphers, :$no-compression,
                  :$no-session-resumption-on-renegotiation --> Supply) {
        # Get the setup work onto another thread.
        supply whenever Promise.kept() {
            my $setup-supply = self!server-setup:
                $socket,
                :$enc, :$version, :$certificate-file, :$private-key-file,
                :$alpn, :$ciphers, :$prefer-server-ciphers, :$no-compression,
                :$no-session-resumption-on-renegotiation;
            whenever $setup-supply {
                .emit;
            }
        }
    }

    method !server-setup($connection-source,
                  :$enc, OpenSSL::ProtocolVersion :$version,
                  :$certificate-file, :$private-key-file, :$alpn,
                  Str :$ciphers, :$prefer-server-ciphers, :$no-compression,
                  :$no-session-resumption-on-renegotiation) {
        sub alpn-selector($ssl, $out, $outlen, $in, $inlen, $arg) {
            my $buf = Buf.new;
            for (0...$inlen-1) {
                $buf.push: $in[$_];
            }
            my $protos = parse-protocol-list($buf, $inlen);
            my $result;

            if $alpn ~~ Callable {
                $result = $alpn($protos);
            } else {
                return SSL_TLSEXT_ERR_NOACK if $alpn.elems == 0;
                for @$protos -> $p {
                    $alpn.map({ if ($_ eq $p) { $result = $p; } });
                    last if $result;
                }
            }

            return SSL_TLSEXT_ERR_ALERT_FATAL unless $result;
            $out[0] = CArray[uint8].new($result.encode('ascii').list);
            $outlen[0] = $result.chars;
            SSL_TLSEXT_ERR_OK;
        }

        supply {
            # Build context, which we'll share between connections.
            my $ctx;
            $lib-lock.protect: {
                $ctx = self!build-server-ctx($version);
                my ($have-cert, $have-pkey);
                with $certificate-file {

                    die X::IO::Socket::Async::SSL.new(
                            message => "can not read certificate-file: $certificate-file"
                        ) unless $certificate-file.IO.r;

                    if 1 == OpenSSL::Ctx::SSL_CTX_use_certificate_chain_file($ctx,
                        $certificate-file.Str)
                    {
                        $have-cert = 'PEM';
                    } elsif 1 == OpenSSL::Ctx::SSL_CTX_use_certificate_file($ctx,
                            $certificate-file.Str, 2)
                    {
                        $have-cert = 'DER';
                    } else {
                        # Failed to import either PEM chain or ASN1 certificate file
                        # Proceeding with PKCS12
                        my $p12buf = slurp $certificate-file, :bin;
                        my Pointer $pkcs12 = d2i_PKCS12(Pointer,
                            CArray[CArray[uint8]].new([CArray[uint8].new($p12buf)]),
                            $p12buf.elems);
                        die "Failed to import $certificate-file as PEM/ASN1/PKCS12"
                            unless so $pkcs12;
                        my $pkey = CArray[Pointer].new([Pointer.new]);
                        my $cert = CArray[Pointer].new([Pointer.new]);
                        my $chain = CArray[Pointer].new([Pointer.new]);
                        # TODO: Passphrase handling
                        die "Failed to parse $certificate-file as PKCS12"
                            unless 1 == PKCS12_parse($pkcs12, '', $pkey, $cert, $chain);
                        if so $pkey[0] {
                            $have-pkey = 'PKCS12';
                            OpenSSL::Ctx::SSL_CTX_use_PrivateKey($ctx, $pkey[0]);
                            OpenSSL::EVP::EVP_PKEY_free($pkey[0]);
                        }
                        if so $cert[0] {
                            $have-cert = 'PKCS12';
                            OpenSSL::Ctx::SSL_CTX_use_certificate($ctx, $cert[0]);
                            OpenSSL::X509::X509_free($cert[0]);
                        }
                        if so $chain[0] {
                            for (0..OpenSSL::Stack::sk_num(nativecast(OpenSSL::Stack, $chain[0]))) {
                                my $x509 = OpenSSL::Stack::sk_value(nativecast(OpenSSL::Stack, $chain[0]), $_);
                                # #define SSL_CTX_add_extra_chain_cert(ctx,x509) \
                                #       SSL_CTX_ctrl(ctx,SSL_CTRL_EXTRA_CHAIN_CERT,0,(char *)x509)
                                if so $x509 {
                                    OpenSSL::Ctx::SSL_CTX_ctrl($ctx, 14, 0, $x509);
                                }
                            }
                            OpenSSL::Stack::sk_free(nativecast(OpenSSL::Stack, $chain[0]));
                        }
                    }
                    die "No server certificate in $certificate-file" without $have-cert;
                }
                with $private-key-file {
                    die "Private key already added as $have-pkey" with $have-pkey;
                    OpenSSL::Ctx::SSL_CTX_use_PrivateKey_file($ctx,
                        $private-key-file.Str, 1);
                }
                with get_dh2048() {
                    if SSL_CTX_ctrl_DH($ctx, SSL_CTRL_SET_TMP_DH, 0, $_) == 0 {
                        warn "IO::Socket::Async::SSL: Failed to set temporary DH";
                    }
                }
                else {
                    warn "IO::Socket::Async::SSL: Failed to create DH";
                }
                with get_ecdh() {
                    if SSL_CTX_ctrl_ECDH($ctx, SSL_CTRL_SET_TMP_ECDH, 0, $_) == 0 {
                        warn "IO::Socket::Async::SSL: Failed to set temporary ECDH";
                    }
                }
                else {
                    warn "IO::Socket::Async::SSL: Failed to create ECDH";
                }
                with $ciphers {
                    if SSL_CTX_set_cipher_list($ctx, $ciphers) == 0 {
                        die "No ciphers from the provided list were selected";
                    }
                }
                if $prefer-server-ciphers {
                    OpenSSL::Ctx::SSL_CTX_ctrl($ctx, SSL_CTRL_OPTIONS,
                        SSL_OP_CIPHER_SERVER_PREFERENCE, Str);
                }
                if $no-compression {
                    OpenSSL::Ctx::SSL_CTX_ctrl($ctx, SSL_CTRL_OPTIONS,
                        SSL_OP_NO_COMPRESSION, Str);
                }
                if $no-session-resumption-on-renegotiation {
                    OpenSSL::Ctx::SSL_CTX_ctrl($ctx, SSL_CTRL_OPTIONS,
                        SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION, Str);
                }

                if $alpn.defined {
                    SSL_CTX_set_alpn_select_cb(
                        $ctx,
                        &alpn-selector,
                        Pointer);
                }
            }

            CLOSE {
                $lib-lock.protect: {
                    if $ctx {
                        OpenSSL::Ctx::SSL_CTX_free($ctx);
                        $ctx = Nil;
                    }
                }
            }

            if $connection-source ~~ Supply {
                whenever $connection-source -> $sock {
                    handle-connection($sock);
                }
            }
            else {
                handle-connection($connection-source);
            }

            sub handle-connection($sock) {
                my $accepted-promise = Promise.new;
                $lib-lock.protect: {
                    with $ctx {
                        my $ssl = OpenSSL::SSL::SSL_new($ctx);
                        my $read-bio = BIO_new(BIO_s_mem());
                        my $write-bio = BIO_new(BIO_s_mem());
                        check($ssl, OpenSSL::SSL::SSL_set_bio($ssl, $read-bio, $write-bio));
                        OpenSSL::SSL::SSL_set_accept_state($ssl);
                        CATCH {
                            .note;
                            OpenSSL::SSL::SSL_free($ssl) if $ssl;
                            OpenSSL::Ctx::SSL_CTX_free($ctx) if $ctx;
                        }
                        self.bless(
                            :$sock, :$enc, :$ssl, :$read-bio, :$write-bio,
                            :$accepted-promise, :$alpn
                        )
                    } else {
                        $accepted-promise.break;
                    }
                }
                whenever $accepted-promise -> $ssl-socket {
                    emit $ssl-socket;
                    QUIT {
                        default {
                            # If the handshake failed, drop the connection.
                            $sock.close;
                        }
                    }
                }
            }
        }
    }

    method !build-server-ctx($version) {
        my $method = do given $version {
            when 2 { OpenSSL::Method::SSLv2_server_method() }
            when 3 { OpenSSL::Method::SSLv3_server_method() }
            when 1 { OpenSSL::Method::TLSv1_server_method() }
            when 1.1 { OpenSSL::Method::TLSv1_1_server_method() }
            when 1.2 { OpenSSL::Method::TLSv1_2_server_method() }
            default {
                try { OpenSSL::Method::TLSv1_2_server_method() } ||
                    try { OpenSSL::Method::TLSv1_server_method() }
            }
        }
        OpenSSL::Ctx::SSL_CTX_new($method)
    }

    method !handle-buffers() {
        if !$!ssl {
            # Connection no longer active; don't do anything.
        }
        elsif $!connected-promise || $!accepted-promise {
            loop {
                my $buf = buf8.allocate(32768);
                my $bytes-read = OpenSSL::SSL::SSL_read($!ssl, $buf, 32768);
                if $bytes-read > 0 {
                    $!bytes-received.emit($buf.subbuf(0, $bytes-read));
                }
                elsif $bytes-read == 0 {
                    last;
                }
                else {
                    check($!ssl, $bytes-read);
                    last;
                }
            }
            with $!shutdown-promise {
                if check($!ssl, OpenSSL::SSL::SSL_shutdown($!ssl)) >= 0 {
                    $!shutdown-promise.keep(True) unless $!shutdown-promise;
                }
                self!flush-read-bio();
            }
            CATCH {
                default {
                    $!bytes-received.quit($_);
                }
            }
        }
        orwith $!connected-promise {
            if check($!ssl, OpenSSL::SSL::SSL_connect($!ssl), 1) > 0 {
                # ALPN check
                if $!alpn.defined && $!alpn-result !~~ Nil|Buf {
                    self!check-alpn;
                } else {
                    $!alpn-result := Nil;
                }
                if $!insecure {
                    $!connected-promise.keep(self) if $!connected-promise.status ~~ Planned;
                }
                else {
                    my $cert = SSL_get_peer_certificate($!ssl);
                    if $cert {
                        if self!hostname-mismatch($cert) -> $message {
                            $!connected-promise.break(X::IO::Socket::Async::SSL::Verification.new(
                                :$message
                            ));
                        }
                        elsif (my $verify = SSL_get_verify_result($!ssl)) == 0 {
                            $!connected-promise.keep(self);
                        }
                        else {
                            my $reason = %VERIFY_FAILURE_REASONS{$verify} // 'unknown failure';
                            $!connected-promise.break(X::IO::Socket::Async::SSL::Verification.new(
                                message => "Server certificate verification failed: $reason"
                            ));
                        }
                    }
                    else {
                        $!connected-promise.break(X::IO::Socket::Async::SSL::Verification.new(
                            message => 'Server did not provide a certificate to verify'
                        ));
                    }
                }
            }
            self!flush-read-bio();
            CATCH {
                default {
                    if $!connected-promise {
                        $!bytes-received.quit($_);
                    }
                    else {
                        $!connected-promise.break($_);
                    }
                }
            }
        }
        orwith $!accepted-promise {
            if check($!ssl, OpenSSL::SSL::SSL_accept($!ssl)) >= 0 {
                # ALPN
                if $!alpn.defined && $!alpn-result !~~ Nil|Buf {
                    self!check-alpn;
                } else {
                    $!alpn-result := Nil;
                }
                $!accepted-promise.keep(self) if $!accepted-promise.status ~~ Planned;
            }
            self!flush-read-bio();
            CATCH {
                default {
                    if $!accepted-promise {
                        $!bytes-received.quit($_);
                    }
                    else {
                        $!accepted-promise.break($_) if $!accepted-promise.status ~~ Planned;
                    }
                }
            }
        }
    }

    method !check-alpn() {
        my $protocol = CArray[CArray[uint8]].new;
        $protocol[0] = CArray[uint8].new;
        my uint32 $len;
        SSL_get0_alpn_selected($!ssl, $protocol, $len);
        if $len == 0 {
            $!alpn-result = Nil;
        } else {
            for (0...$len-1) {
                $!alpn-result ~= chr($protocol[0][$_]);
            }
        }
    }

    method !flush-read-bio(--> Nil) {
        my $buf = Buf.allocate(32768);
        while OpenSSL::Bio::BIO_read($!write-bio, $buf, 32768) -> $bytes-read {
            last if $bytes-read < 0;
            my $p = $!sock.write($buf.subbuf(0, $bytes-read));
            @!outstanding-writes.push($p);
            $p.then: {
                $lib-lock.protect: {
                    @!outstanding-writes .= grep({ $_ !=== $p });
                }
            }
        }
    }

    method !hostname-mismatch($cert) {
        my $altnames = X509_get_ext_d2i($cert, NID_subject_alt_name, CArray[int32], CArray[int32]);
        my $fold-host = $!host.fc;
        if ($altnames) {
            my @no-match;
            loop (my int $i = 0; $i < $altnames.num; $i++) {
                my $gd = nativecast(GENERAL_NAME, $altnames.data[$i]);
                my $out = CArray[CArray[uint8]].new;
                $out[0] = CArray[uint8];
                my $name-bytes = ASN1_STRING_to_UTF8($out, $gd.data);
                my $name = Buf.new($out[0][^$name-bytes]).decode('utf-8');
                given $gd.type {
                    when GEN_DNS {
                        my $fold-name = $name.fc;
                        return if $fold-name eq $fold-host ||
                                  wildcard-match($fold-name, $fold-host);
                        push @no-match, $name;
                    }
                    # TODO IP address case
                }
            }
            if @no-match {
                return "Host $!host does not match any subject alt name on the " ~
                    "certificate (@no-match.join(', '))";
            }
        }
        else {
            # TODO Common names fallback
            return "Certificate contains no altnames to check host against";
        }
        Nil
    }

    # Implements the rules from RFC 6125 section 6.4.3.
    sub wildcard-match($name, $host) {
        return False without $name.index('*');
        my ($name-wild, $rest-name) = $name.split('.', 2);
        my ($host-wild, $rest-host) = $host.split('.', 2);
        return False unless $rest-name eq $rest-host;
        if $name-wild eq '*' {
            return True;
        }
        elsif $host-wild.chars < $name-wild.chars - 1 {
            # fo*od can match foxod or food but never fod.
            return False;
        }
        elsif $name-wild ~~ /^ (<-[*]>*) '*' (<-[*]>*) $/ {
            return $host-wild.starts-with(~$0) &&
                   $host-wild.ends-with(~$1);
        }
        return False;
    }

    sub parse-protocol-list($array, $len --> List) {
        my @result;
        my $names = $array;
        my $rest = $len;
        while $rest {
            my $size = $names[0];
            @result.push: $names.subbuf(1, $size).decode;
            $names .= subbuf($size+1);
            $rest -= $size + 1;
        }
        @result;
    }

    sub build-protocol-list(@protocols --> Buf) {
        my $list = Buf.new;
        for @protocols -> $p {
            $list.push: $p.chars;
            $list.push: $p.encode('ascii')
        }
        $list;
    }

    my constant SSL_ERROR_WANT_READ = 2;
    my constant SSL_ERROR_WANT_WRITE = 3;
    sub check($ssl, $rc, $expected = 0) {
        if $rc < $expected {
            my $error = OpenSSL::Err::ERR_get_error();
            my @log;
            while ($error != 0|SSL_ERROR_WANT_READ|SSL_ERROR_WANT_WRITE) {
                @log.push(OpenSSL::Err::ERR_error_string($error, Nil));
                $error = OpenSSL::Err::ERR_get_error();
            }
            if @log.elems != 0 {
                die X::IO::Socket::Async::SSL.new(
                    message => @log.join("\n")
                )
            }
        }
        $rc
    }

    #| Get a Supply of incoming data, either as a byte buffer if the :bin
    #| option is passed, or as strings otherwise. Note that strings will,
    #| in applicable encodings, be produced in NFG.
    method Supply(:$bin, :$enc = $!enc, :$scheduler = $*SCHEDULER --> Supply) {
        if $bin {
            $!bytes-received.Supply.Channel.Supply
        }
        else {
            supply {
                my $norm-enc = Rakudo::Internals.NORMALIZE_ENCODING($enc // 'utf-8');
                my $dec = Encoding::Registry.find($norm-enc).decoder();
                whenever $!bytes-received.Supply.Channel.Supply {
                    $dec.add-bytes($_);
                    emit $dec.consume-available-chars();
                    LAST if $dec.consume-all-chars() -> $final-chars {
                        emit $final-chars;
                    }
                }
            }
        }
    }

    #| Encode a string and send its bytes over the TLS connection. Returns
    #| a Promise that will be kept if the data is sent, and broken in the
    #| case of an error.
    method print(IO::Socket::Async::SSL:D: Str() $str, :$scheduler = $*SCHEDULER --> Promise) {
        self.write($str.encode($!enc // 'utf-8'), :$scheduler)
    }

    #| Send the bytes in the passed blob over the TLS connection. Returns
    #| a Promise that will be kept if the data is sent, and broken in the
    #| case of an error.
    method write(IO::Socket::Async::SSL:D: Blob $b, :$scheduler = $*SCHEDULER) {
        $lib-lock.protect: {
            if $!closed {
                my $p = Promise.new;
                $p.break(X::IO::Socket::Async::SSL.new(
                    message => 'Cannot write to closed socket'
                ));
                return $p;
            }
            my $ret = OpenSSL::SSL::SSL_write($!ssl, $b, $b.bytes);
            my $p = start {
                $lib-lock.protect: {
                    self!flush-read-bio();
                    # The following doesn't race on $p assignment due to the
                    # holding of $lib-lock in the code with the assignment.
                    @!outstanding-writes .= grep({ $_ !=== $p });
                }
                $ret
            }
            @!outstanding-writes.push($p);
            $p
        }
    }

    #| Get the peer (remote) host
    method peer-host() {
        $!sock.peer-host;
    }

    #| Get the peer (remote) port
    method peer-port() {
        $!sock.peer-port;
    }

    #| Get the socket (local) host
    method socket-host() {
        $!sock.socket-host;
    }

    #| Get the socket (local) port
    method socket-port() {
        $!sock.socket-port;
    }

    #| Get the socket native descriptor
    method native-descriptor() {
        $!sock.native-descriptor;
    }

    #| Closes the connection. This will await the completion of any
    #| outstanding writes before closing.
    method close(IO::Socket::Async::SSL:D: --> Nil) {
        my @wait-writes;
        $lib-lock.protect: {
            $!closed = True;
            if @!outstanding-writes {
                @wait-writes = @!outstanding-writes;
            }
            else {
                return if $!shutdown-promise;
                without $!shutdown-promise {
                    $!shutdown-promise = Promise.new;
                    $!shutdown-promise.then: {
                        $!sock.close;
                        self!cleanup();
                    }
                    self!handle-buffers();
                }
            }
        }
        if @wait-writes {
            Promise.allof(@wait-writes).then({ self.close });
        }
        else {
            await $!shutdown-promise;
        }
    }

    my $alpn;

    #| Check if ALPN support is available
    method supports-alpn(--> Bool) {
        $alpn //= so try {
            my $ctx = self!build-client-ctx(-1);
            my $buf = build-protocol-list(['h2']);
            SSL_CTX_set_alpn_protos($ctx, $buf, $buf.elems);
            LEAVE OpenSSL::Ctx::SSL_CTX_free($ctx) if $ctx;
            True
        }
    }

    method DESTROY() {
        self!cleanup();
    }

    method !cleanup() {
        if $!ssl || $!ctx {
            start $lib-lock.protect: {
                if $!ssl {
                    OpenSSL::SSL::SSL_free($!ssl);
                    $!ssl = Nil;
                }
                if $!ctx {
                    OpenSSL::Ctx::SSL_CTX_free($!ctx);
                    $!ctx = Nil;
                }
                $!read-bio = Nil;
                $!write-bio = Nil;
            }
        }
    }
}

=begin pod

=head2 Bugs, feature requests, and contributions

Please use GitHub Issues to file bug reports and feature requests.
If you wish to contribute to this module, please open a GitHub Pull
Request.

Please send an email to the Raku Security Team (security@raku.org)
to report security vulnerabilities.

=head1 AUTHOR

Jonathan Worthington

=head1 COPYRIGHT AND LICENSE

Copyright 2017 - 2024 Jonathan Worthington

Copyright 2024 - 2025 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
