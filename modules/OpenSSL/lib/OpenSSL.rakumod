unit class OpenSSL;

use OpenSSL::SSL;
use OpenSSL::Bio;
use OpenSSL::Err;
use OpenSSL::EVP;
use OpenSSL::X509;

use NativeCall;

has OpenSSL::Ctx::SSL_CTX $.ctx;
has OpenSSL::SSL::SSL $.ssl;
has $.client;

has $.using-bio = False;
has $.bio-read-buf = buf8.new;
has $.net-write;
has $.net-read;

has $.net-bio;
has $.internal-bio;

class X::OpenSSL::Exception is Exception {
    has $.message;
}

# SSLv2 | SSLv3 | TLSv1 | TLSv1.1 | TLSv1.2 | default
subset ProtocolVersion of Numeric where * == 2| 3| 1| 1.1| 1.2| -1;

method new(Bool :$client = False, ProtocolVersion :$version = -1) {

    # make a simple call to ensure libeay32.dll is loaded before ssleay32.dll (on windows)
    #
    # if we're using our bundled .dll files, and we try to load ssleay32.dll first, LoadLibrary
    # can't find the required libeay32.dll anywhere in the path, and so fails to load the dll
    OpenSSL::EVP::EVP_aes_128_cbc();
    
    # Provisional support for OpenSSL 1.1
    try {
        CATCH {
            default { OpenSSL::SSL::OPENSSL_init_ssl(0, OpaquePointer); }
        }
        OpenSSL::SSL::SSL_library_init();
        OpenSSL::SSL::SSL_load_error_strings();
    }

    my $method;
    given $version {
        when 2 {
            $method = $client
              ?? OpenSSL::Method::SSLv2_client_method()
              !! OpenSSL::Method::SSLv2_server_method();
        }
        when 3 {
            $method = $client
              ?? OpenSSL::Method::SSLv3_client_method()
              !! OpenSSL::Method::SSLv3_server_method();
        }
        when 1 {
            $method = $client
              ?? OpenSSL::Method::TLSv1_client_method()
              !! OpenSSL::Method::TLSv1_server_method();
        }
        when 1.1 {
            $method = $client
              ?? OpenSSL::Method::TLSv1_1_client_method()
              !! OpenSSL::Method::TLSv1_1_server_method();
        }
        when 1.2 {
            $method = $client
              ?? OpenSSL::Method::TLSv1_2_client_method()
              !! OpenSSL::Method::TLSv1_2_server_method();
        }
        # No explicit version means: negotiate.
        # In OpenSSL 1.1.0, TLS_method() replaces SSLv23_method()
        default {
            $method = try {$client
                            ?? OpenSSL::Method::TLS_client_method()
                            !! OpenSSL::Method::TLS_server_method()
                          } || try {
                              $client
                                ?? OpenSSL::Method::SSLv23_client_method()
                                !! OpenSSL::Method::SSLv23_server_method()
                          }
        }
    }
    my $ctx = OpenSSL::Ctx::SSL_CTX_new( $method );
    my $ssl = OpenSSL::SSL::SSL_new( $ctx );


    self.bless(:$ctx, :$ssl, :$client)
}

method set-server-name(Str $server-name) {
    explicitly-manage($server-name);
    OpenSSL::SSL::SSL_ctrl($!ssl, 55, 0, $server-name)

}

method set-fd(int32 $fd) {
    OpenSSL::SSL::SSL_set_fd($!ssl, $fd)
}

method set-socket(IO::Socket $s) {
    # see http://wiki.openssl.org/index.php/Manual:BIO_s_bio(3)

    $!using-bio = True;
    my $n-ptr = CArray[OpaquePointer].new;
    $n-ptr[0] = OpaquePointer;
    my $i-ptr = CArray[OpaquePointer].new;
    $i-ptr[0] = OpaquePointer;

    my $ret = OpenSSL::Bio::BIO_new_bio_pair($n-ptr, 0, $i-ptr, 0);
    if $ret == 0 {
        my $e = OpenSSL::Err::ERR_get_error();
        say "err code: $e";
        say OpenSSL::Err::ERR_error_string($e);
    }
    $!net-bio = $n-ptr[0];
    $!internal-bio = $i-ptr[0];
    OpenSSL::SSL::SSL_set_bio($!ssl, $.internal-bio, $.internal-bio);

    if $s.?host -> $host {
        self.set-server-name($host);
    }

    $!net-write = -> $buf { $s.write($buf) }

    $!net-read = -> $n = Inf { $s.recv($n, :bin) }

    0
}

method bio-write {
    # if we're handling the network in P6, dump everything we can
    if $.using-bio {
        my $cbuf = buf8.new;
        $cbuf[1024] = 0;
        while (my $len = OpenSSL::Bio::BIO_read($.net-bio, $cbuf, 1024)) > 0 {
            my $buf = $cbuf.subbuf(0, $len);
            $.net-write.($buf);
        }
    }
}
method bio-read {
    # if we're handling the network in P6, read everything we can
    my $read = 0;
    if $.using-bio {
        if $!bio-read-buf.bytes == 0 {
            $!bio-read-buf = $.net-read.();
        }
        $read = $!bio-read-buf.bytes;
        my $bytes = OpenSSL::Bio::BIO_write($.net-bio, $!bio-read-buf, $!bio-read-buf.bytes);
        $!bio-read-buf = $!bio-read-buf.subbuf($bytes);
    }
    $read
}
method handle-error($code) {
    my $e = OpenSSL::SSL::SSL_get_error($!ssl, $code);
    return 0 unless $e;
    my $try-recover = -1;
    if $e == 2 && $.using-bio { # SSL_ERROR_WANT_READ
        $.bio-write;
        my $read = $.bio-read;
        $try-recover = 1 if $read;
    }
    elsif $e == 3 && $.using-bio { # SSL_ERROR_WANT_WRITE
        $.bio-write;
        $try-recover = 1;
    }
    else {
        # we don't know what to do with it - pass the error up the stack
        $try-recover = -1;
    }

    $try-recover
}

method set-connect-state {
    OpenSSL::SSL::SSL_set_connect_state($!ssl);
}

method set-accept-state {
    OpenSSL::SSL::SSL_set_accept_state($!ssl);
}

method connect {
    my $ret;

    loop {
        $ret = OpenSSL::SSL::SSL_connect($!ssl);

        my $e = $.handle-error($ret);
        last unless $e > 0;
    }

    $ret
}

method accept {
    my $ret;

    loop {
        $ret = OpenSSL::SSL::SSL_accept($!ssl);

        my $e = $.handle-error($ret);
        last unless $e > 0;
    }

    $ret
}

multi method write(Str $s) {
    $.write($s.encode)
}

multi method write(Blob $b) {
    my int32 $n = $b.bytes;
    my $ret;

    loop {
        $ret = OpenSSL::SSL::SSL_write($!ssl, $b, $n);

        my $e = $.handle-error($ret);
        last unless $e > 0;
    }

    $.bio-write;

    $ret
}

method read(Int $n, Bool :$bin) {
    my int32 $count = $n;
    my $carray = buf8.allocate($n min 16384);
    my $total-read = 0;
    my $buf = buf8.new;
    loop {
        my $read = OpenSSL::SSL::SSL_read($!ssl, $carray, $count - $total-read);

        if $read > 0 {
            $buf.append($carray.subbuf(0, $read));
            $total-read += $read;
        }

        last if $total-read >= $n;

        my $e = 0;
        $e = $.handle-error($read) if $read < 0;
        last if $e <= 0 || $total-read >= $n;
    }

    $bin ?? $buf !! $buf.decode('latin-1')
}

method use-certificate-file(Str $file) {
    # only PEM file so far : TODO : more file types
    if OpenSSL::Ctx::SSL_CTX_use_certificate_file($!ctx, $file, 1) <= 0 {
        die "Failed to set certificate file";
    }
}

method use-privatekey-file(Str $file) {
    # only PEM file so far : TODO : more file types
    if OpenSSL::Ctx::SSL_CTX_use_PrivateKey_file($!ctx, $file, 1) <= 0 {
        die "Failed to set PrivateKey file";
    }
}

method use-client-ca-file(Str $file, :$debug is copy) {
    unless my $ca-stack = OpenSSL::SSL::SSL_load_client_CA_file(
      CArray[uint8].new( $file.encode.list, 0 )
    ) {
        my $e = OpenSSL::Err::ERR_get_error();
        die X::OpenSSL::Exception.new(
          message => "OpenSSL error $e -- "
            ~ OpenSSL::Err::ERR_error_string( $e, Nil )
        );
    }

    if $debug || %*ENV<OPENSSL_CA_DEBUG> {
        $debug = $*ERR unless $debug ~~ IO::Handle;

        say $debug: "*** OPENSSL CA LIST -- LOADED ***";
        OpenSSL::X509::dump_x509_stack($ca-stack, :FH($debug));
    }

    OpenSSL::SSL::SSL_set_client_CA_list( $!ssl, $ca-stack );

    $ca-stack
}

method get-client-ca-list (:$debug is copy) {
    my $ca-stack = OpenSSL::SSL::SSL_get_client_CA_list( $!ssl );

    if $debug || %*ENV<OPENSSL_CA_DEBUG> {
        $debug = $*ERR unless $debug ~~ IO::Handle;

        say $debug: "*** OPENSSL CA LIST ***";
        OpenSSL::X509::dump_x509_stack($ca-stack, :FH($debug));
    }

    $ca-stack
}

method check-private-key {
    unless OpenSSL::Ctx::SSL_CTX_check_private_key($!ctx) {
        die "Private key does not match the public certificate";
    }
}

method shutdown {
    with $!ssl {
        OpenSSL::SSL::SSL_shutdown($!ssl);
    }
}

method ctx-free {
    with $!ctx {
        OpenSSL::Ctx::SSL_CTX_free($!ctx);
        $!ctx = Nil;
    }
}

method ssl-free {
    with $!ssl {
        OpenSSL::SSL::SSL_free($!ssl);
        if $.using-bio {
            # $.internal-bio is freed by the SSL_free call
            OpenSSL::Bio::BIO_free($.net-bio);
        }
        $!ssl = Nil;
    }
}

method close {
    until self.shutdown {};
    self.ssl-free;
    self.ctx-free;
    1
}

# vim: expandtab shiftwidth=4
